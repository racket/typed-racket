#lang racket/unit

;; This module provides a unit for type-checking units
;; The general strategy for typechecking all of the racket/unit forms
;; is to match the entire expanded syntax and parse out the relevant
;; pieces of information.
;;
;; Each typing rule knows the expected expansion of the form being checked
;; and specifically parses that syntax. This implementation is extremely
;; brittle and will require changes should the expansion of any of the unit
;; forms change.
;;
;; For unit forms the general idea is to parse expanded syntax to find information
;; related to:
;; - imports
;; - exports
;; - init-depend
;; - subexpressions that require typechecking
;; And use these pieces to typecheck the entire form
;;
;; For the `unit` form imports, exports, and init-depends are parsed to generate
;; the type of the expression and to typecheck the body of the unit since imported signatures
;; introduce bindings of variables to types, and exported variables must be defined
;; with subtypes of their expected types.
;;
;; The `invoke-unit` expansion is more complex and depends on whether or not
;; imports were specified. In the case of no imports, the strategy is simply to
;; find the expression being invoked and ensure it has the type of a unit with
;; no imports. When there are imports to an `invoke-unit` form, the syntax contains
;; local definitions of units defined using `unit-from-context`. These forms
;; are parsed to determine which imports were declared to check subtyping on the
;; invoked expression and to ensure that imports pulled from the current context
;; have the correct types.
;;
;; The `compound-unit` expansion contains information about the imports and exports
;; of each unit expression being linked. Additionally the typed `compound-unit` macro
;; attaches a syntax property that specifies the exact linking structure of the compound
;; unit. These pieces of information enable the calculation of init-depends for the entire
;; compound unit and to properly check subtyping on each linked expression.
;;
;; `unit-from-context` is handled similarly to `invoke-unit`, the expansion is exactly
;; that of a unit created using the `unit` form, but lacks the annotations that are placed
;; there by the typed `unit` macro. In this case the body of the unit is searched for
;; syntax corresponding to definitions which are checked against the declared exports
;; to ensure the form is well typed.
;;
;; The handling of the various `infer` forms (invoke-unit/infer compound-unit/infer)
;; is generally identical to the corresponding form lacking inference, however, in these
;; cases typechecking can be more lax. In particular, the unit implementation knows that
;; only valid unit expressions are used in these forms and so there is no need to typecheck
;; each unit subexpression unless it is needed to determine the result type. The
;; `compund-unit/infer` form, however, requires the cooperation of the unit implementation
;; to attach a syntax property that specified the init-depends of the compound unit, otherwise
;; this information is extremely difficult to obtain from the syntax alone.

(require "../utils/utils.rkt"
         syntax/id-set
         racket/set
         racket/dict
         racket/format
         racket/list
         racket/match
         racket/syntax
         syntax/id-table
         syntax/parse
         syntax/stx
         syntax/strip-context
         racket/unit-exptime
         "signatures.rkt"
         (private parse-type syntax-properties type-annotation)
         (only-in (base-env base-special-env) make-template-identifier)
         (env lexical-env tvar-env global-env 
              signature-env)
         (types utils abbrev union subtype resolve generalize signatures)
         (typecheck check-below internal-forms)
         (utils tc-utils)
         (rep type-rep)
         (for-syntax racket/base racket/unit-exptime syntax/parse)
         (for-template racket/base
                       racket/unsafe/undefined
                       (submod "internal-forms.rkt" forms)))

(import tc-let^ tc-expr^)
(export check-unit^)

;; Syntax class definitions
;; variable annotations are modified by the expansion of the typed unit
;; macro in order to allow annotations on exported variables, this
;; syntax class allows conversion back to the usual internal syntax
;; for type annotations which may be used by tc-letrec/values
(define-syntax-class unit-body-annotation
  #:literal-sets (kernel-literals)
  #:literals (void values :-internal cons)
  (pattern
   (#%expression
    (begin
      (#%plain-app void (#%plain-lambda () var-int))
      (begin
        (quote-syntax
         (:-internal var:id t) #:local)
        (#%plain-app values))))
   #:attr name #'var
   #:attr fixed-form (quasisyntax/loc this-syntax
                       (begin
                           (quote-syntax (:-internal var-int t) #:local)
                           (#%plain-app values)))))

;; Syntax class matching the syntax of the rhs of definitions within unit bodies
;; The typed unit macro attaches the lambda to allow unit typechecking to associate
;; variable names with their definitions which are otherwise challenging to recover
(define-syntax-class unit-body-definition
  #:literal-sets (kernel-literals)
  #:literals (void)
  (pattern
   (#%expression
    (begin
      (#%plain-app void (#%plain-lambda () var:id ... (#%plain-app void)))
      e))
   #:with vars #'(var ...)
   #:with body #'e))

;; Process the syntax of annotations and definitions from a unit
;; produces two values representing the names and the exprs corresponding
;; to each definition or annotation
;; Note:
;; - definitions may produce multiple names via define-values
;; - annotations produce no names
(define (process-ann/def-for-letrec ann/defs)
  (for/fold ([names #`()]
             [exprs #`()])
            ([a/d (in-list ann/defs)])
    (syntax-parse a/d
      [a:unit-body-annotation
       (define name (attribute a.name))
       ;; TODO:
       ;; Duplicate annotations from imports
       ;; are not currently detected due to a bug
       ;; in tc/letrec-values
       ;; See Problem Report: 15145
       (define fixed (attribute a.fixed-form))
       (values #`(#,@names ()) #`(#,@exprs #,fixed))]
      [d:unit-body-definition
       (values #`(#,@names d.vars) #`(#,@exprs d.body))])))

;; A Sig-Info is a (sig-info identifier? (listof identifier?) (listof identifier?))
;; name is the identifier corresponding to the signature this sig-info represents
;; externals is the list of external names for variables in the signature
;; internals is the list of internal names for variables in the signature
;; Note:
;; - external names are those attached to signatures stored in static information
;;   and in the Siganture representation
;; - internal names are the internal renamings of those variables in fully expanded
;;   unit syntax, this renaming is performed by the untyped unit macro
;; - All references within a unit body use the internal names
(struct sig-info (name externals internals) #:transparent)

;; Process the various pieces of the fully expanded unit syntax to produce
;; sig-info structures for the unit's imports and exports, and a list of the
;; identifiers corresponding to init-depends of the unit
(define (process-unit-syntax import-sigs import-internal-ids import-tags
                             export-sigs export-temp-ids export-temp-internal-map
                             init-depend-tags)
  ;; build a mapping of import-tags to import signatures
  ;; since init-depends are referenced by the tags only in the expanded syntax
  ;; this map is used to determine the actual signatures corresponding to the
  ;; given signature tags of the init-depends
  (define tag-map (make-immutable-free-id-table (map cons import-tags import-sigs)))
  (define lookup-temp (λ (temp) (free-id-table-ref export-temp-internal-map temp #f)))
  
  (values (for/list ([sig-id  (in-list import-sigs)]
                     [sig-internal-ids (in-list import-internal-ids)])
            (sig-info sig-id
                      (map car (Signature-mapping (lookup-signature/check sig-id)))
                      sig-internal-ids))
          ;; export-temp-ids is a flat list which must be processed
          ;; sequentially to map them to the correct internal/external identifiers
          (let-values ([(_ si)
                        (for/fold ([temp-ids export-temp-ids]
                                   [sig-infos '()])
                                  ([sig (in-list export-sigs)])
                          (define external-ids
                            (map car (Signature-mapping (lookup-signature/check sig))))
                          (define len (length external-ids))
                          (values (drop temp-ids len)
                                  (cons (sig-info sig
                                                  external-ids
                                                  (map lookup-temp (take temp-ids len)))
                                        sig-infos)))])
            (reverse si))
          (map (λ (x) (free-id-table-ref tag-map x #f)) init-depend-tags)))

;; The following three syntax classes are used to parse specific pieces of
;; information from parts of the expansion of units

;; Needed to parse out signature names, and signature-tags from the unit syntax
;; the tags are used to lookup init-depend signatures
(define-syntax-class sig-vector
  #:literal-sets (kernel-literals)
  #:literals (vector-immutable cons)
  (pattern (#%plain-app 
            vector-immutable
            (#%plain-app cons 
                         (quote sig:id)
                         (#%plain-app vector-immutable sig-tag tag-rest ...))
            ...)
           #:with sigs #'(sig ...)
           #:with sig-tags #'(sig-tag ...)))

(define-syntax-class init-depend-list
  #:literal-sets (kernel-literals)
  #:literals (list cons)
  (pattern (#%plain-app list (#%plain-app cons _ sig-tag) ...)
           #:with init-depend-tags #'(sig-tag ...)))

(define-syntax-class export-table
  #:literal-sets (kernel-literals)
  #:literals (make-immutable-hash list cons vector-immutable check-not-unsafe-undefined unbox)
  (pattern (#%plain-app
            make-immutable-hash
            (#%plain-app
             list
             (#%plain-app
              cons
              signature-tag:id
              (#%plain-app
               vector-immutable
               (#%plain-lambda () 
                 (#%plain-app check-not-unsafe-undefined (#%plain-app unbox export-temp-id) external-id))
               ...))
             ...))
           #:attr export-temp-ids (map syntax->list (syntax->list #'((export-temp-id ...) ...)))))

;; This syntax class matches the whole expansion of unit forms
(define-syntax-class unit-expansion
  #:literal-sets (kernel-literals)
  #:attributes (body-stx 
                import-internal-ids 
                import-sigs 
                import-sig-tags 
                export-sigs 
                export-temp-ids
                init-depend-tags)
  (pattern (#%plain-app
            make-unit:id
            name:expr 
            import-vector:sig-vector
            export-vector:sig-vector
            list-dep:init-depend-list
            (let-values (_ ...)
              (let-values (_ ...)
                (#%expression
                 (#%plain-lambda ()
                   (let-values (((export-temp-id:id) _) ...)
                     (#%plain-app
                      values
                      (#%plain-lambda (import-table:id)
                                      (let-values (((import:id ...) _) ...)
                                        unit-body:expr))
                      et:export-table
                      _ ...)))))))
           #:attr import-sigs (syntax->list #'import-vector.sigs)
           #:attr import-sig-tags (syntax->list #'import-vector.sig-tags)
           #:attr export-sigs (syntax->list #'export-vector.sigs)
           #:attr export-temp-ids (syntax->list #'(export-temp-id ...))
           #:attr init-depend-tags (syntax->list #'list-dep.init-depend-tags)
           #:attr import-internal-ids (map syntax->list (syntax->list #'((import ...) ...)))
           #:with body-stx #'unit-body))

;; Extract the identifiers referenced in unit-from-context and invoke-unit forms
;; in order to typecheck them in the current environment
 (define (extract-definitions stx)
   (trawl-for-property
    stx
    (lambda (stx) (syntax-parse stx [((int:id) ref:id) #t] [_ #f]))
    (lambda (stx) (syntax-parse stx [((int:id) ref:id) #'ref]))))

;; Syntax inside the expansion of units that allows recovering a mapping
;; from temp-ids of exports to their internal identifiers
(define-syntax-class export-temp-internal-map-elem
  #:literal-sets (kernel-literals)
  #:literals (set-box!)
  (pattern (#%plain-app set-box! temp-id:id internal-id:id)))

(define export-map-elem?
  (syntax-parser [e:export-temp-internal-map-elem #t]
                 [_ #f]))
(define extract-export-map-elem
  (syntax-parser [e:export-temp-internal-map-elem (cons #'e.temp-id #'e.internal-id)]))

;; get a reference to the actual `invoke-unit/core` function to properly parse
;; the fully expanded syntax of `invoke-unit` forms
(define invoke-unit/core (make-template-identifier 'invoke-unit/core 'racket/unit))

;; Syntax class for all the various expansions of invoke-unit forms
;; This also includes the syntax for the invoke-unit/infer forms
(define-syntax-class invoke-unit-expansion
  #:literal-sets (kernel-literals)
  (pattern (#%plain-app iu/c unit-expr)
           #:when (free-identifier=? #'iu/c invoke-unit/core)
           #:attr units '()
           #:attr expr #'unit-expr
           #:attr imports '())
  (pattern
   (let-values ()
     body:invoke-unit-linkings)
   #:attr units (attribute body.units)
   #:attr expr (attribute body.expr)
   #:attr imports (attribute body.imports)))

(define-syntax-class invoke-unit-linkings
  #:literal-sets (kernel-literals)
  (pattern
   (let-values ([(u-temp:id)
                 (let-values ([(deps) _]
                              [(sig-provider) _] ...
                              [(temp) ie:invoked-expr])
                   _ ...)])
     (#%plain-app iu/c (#%plain-app values _)))
   #:when (free-identifier=? #'iu/c invoke-unit/core)
   #:attr units '()
   #:attr expr (if (tr:unit:invoke:expr-property #'ie) #'ie #'ie.invoke-expr)
   #:attr imports '())
  (pattern
   (let-values ([(temp-id) u:unit-expansion])
     rest:invoke-unit-linkings)
   #:attr units (cons #'u (attribute rest.units))
   #:attr expr (attribute rest.expr)
   #:attr imports (append (attribute u.export-sigs) (attribute rest.imports))))

;; This should be used ONLY when an invoke/infer is used with the link clause ...
(define-syntax-class invoked-expr
  #:literal-sets (kernel-literals)
  #:literals (values)
  (pattern
   (let-values ([(deps2:id) _]
                [(local-unit-id:id) unit:id] ...
                [(invoke-temp) invoke-unit])
     _ ...)
   #:attr invoke-expr #'invoke-unit)
  (pattern invoke-expr:expr))

;; Compound Unit syntax classes
(define-syntax-class compound-unit-expansion
  #:literal-sets (kernel-literals)
  #:literals (vector-immutable cons)
  (pattern 
   (let-values ([(deps:id) _]
                [(local-unit-name) unit-expr] ...)
     (~seq (#%plain-app check-unit _ ...)
           (#%plain-app check-sigs _
                        (#%plain-app 
                         vector-immutable
                         (#%plain-app cons (quote import-sig:id) _) ...)
                        (#%plain-app
                         vector-immutable
                         (#%plain-app cons (quote export-sig:id) _) ...)
                        _)
           (let-values ([(fht) _]
                        [(rht) _])
             _ ...)) ...
     (#%plain-app
            make-unit:id
            name:expr 
            import-vector:sig-vector
            export-vector:sig-vector
            deps-ref
            internals))
   #:attr unit-exprs (syntax->list #'(unit-expr ...))
   #:attr unit-imports (map syntax->list (syntax->list #'((import-sig ...) ...)))
   #:attr unit-exports (map syntax->list (syntax->list #'((export-sig ...) ...)))
   #:attr compound-imports (syntax->list #'import-vector.sigs)
   #:attr compound-exports (syntax->list #'export-vector.sigs)))

;; A cu-expr-info represents an element of the link clause in
;; a compound-unit form
;; - expr : the unit expression being linked
;; - import-sigs : the Signatures specified as imports for this link-element
;; - import-links : the symbols that correspond to the link-bindings
;;                  imported by this unit
;; - export-sigs : the Signatures specified as exports for this link-element
;; - export-links : the symbols corresponding to the link-bindings exported
;;                  by this unit
(struct cu-expr-info (expr import-sigs import-links export-sigs export-links)
  #:transparent)

;; parse-compound-unit : Syntax -> (Values (Listof (Cons Symbol Id))
;;                                         (Listof Symbol)
;;                                         (Listof Signature)
;;                                         (Listof Signature)
;;                                         (Listof cu-expr-info))
;; Returns a mapping of link-ids to sig-ids, a list of imported sig ids
;; a list of exported link-ids
(define (parse-compound-unit stx)
  (define (list->sigs l) (map lookup-signature/check l))
  (syntax-parse stx
    [cu:compound-unit-expansion
     (define link-binding-info (tr:unit:compound-property stx))
     (match-define (list cu-import-syms unit-export-syms unit-import-syms)
       link-binding-info)
     (define compound-imports (attribute cu.compound-imports))
     (define compound-exports (attribute cu.compound-exports))
     (define unit-exprs (attribute cu.unit-exprs))
     (define unit-imports (attribute cu.unit-imports))
     (define unit-exports (attribute cu.unit-exports))
     ;; Map signature ids to link binding symbols
     (define mapping
       (let ()
         (define link-syms (append cu-import-syms (flatten unit-export-syms)))
         (define sig-ids (append compound-imports (flatten unit-exports)))
         (map cons link-syms (map lookup-signature/check sig-ids))))
     (define cu-exprs
       (for/list ([unit-expr (in-list unit-exprs)]
                  [import-sigs (in-list unit-imports)]
                  [import-links (in-list unit-import-syms)]
                  [export-sigs (in-list unit-exports)]
                  [export-links (in-list unit-export-syms)])
         (cu-expr-info unit-expr
                       (list->sigs import-sigs) import-links
                       (list->sigs export-sigs) export-links)))
     (values 
      mapping
      cu-import-syms
      (list->sigs compound-imports)
      (list->sigs compound-exports)
      cu-exprs)]))

;; Sig-Info -> (listof (pairof identifier? Type))
;; GIVEN: signature information
;; RETURNS: a mapping from internal names to types
(define (make-local-type-mapping si)
  (define sig (lookup-signature/check (sig-info-name si)))
  (define internal-names (sig-info-internals si))
  (define sig-types 
    (map cdr (Signature-mapping sig)))
  (map cons internal-names sig-types))

;; Syntax Option<TCResults> -> TCResults
;; Type-check a unit form
(define (check-unit form [expected #f])
  (define expected-type
    (match expected
      [(tc-result1: type) (resolve type)]
      [_ #f]))
  (match expected-type
    [(? Unit? unit-type)
     (ret (parse-and-check-unit form unit-type))]
    [_ (ret (parse-and-check-unit form #f))]))

;; Syntax Option<TCResultss> -> TCResults

(define (check-invoke-unit form [expected #f])
  (define expected-type 
    (match expected
      [(tc-result1: type) (resolve type)]
      [_ #f]))
  (ret (parse-and-check-invoke form expected-type)))

(define (check-compound-unit form [expected #f])
  (define infer? (eq? (tr:unit:compound-property form) 'infer))
  (define expected-type
    (match expected
      [(tc-result1: type) (resolve type)]
      [_ #f]))
  (if infer?
      (ret (parse-and-check-compound/infer form expected-type))
      (ret (parse-and-check-compound form expected-type))))

(define (check-unit-from-context form [expected #f])
  (define expected-type
    (match expected
      [(tc-result1: type) (resolve type)]
      [_ #f]))
  (ret (parse-and-check-unit-from-context form expected-type)))

(define (parse-and-check-unit-from-context form expected-type)
  (syntax-parse form
    [u:unit-expansion
     (define export-sigs (map lookup-signature/check (attribute u.export-sigs)))
     (define body-stx (attribute u.body-stx))
     (for ([sig (in-list export-sigs)])
       (define ids (extract-definitions body-stx))
       (define types (map cdr (Signature-mapping sig)))
       (for ([type (in-list types)]
             [id (in-list ids)])
         (define lexical-type (lookup-type/lexical id))
         (unless (subtype lexical-type type)
           (tc-error/fields "type mismatch in unit-from-context export"
                            "expected" type
                            "given" lexical-type
                            "exported variable" (syntax-e id)
                            "exported-signature" (syntax-e (Signature-name sig))
                            #:stx form
                            #:delayed #t))))
     (-unit null export-sigs null (-values (list -Void)))]))

(define (parse-and-check-compound form expected-type)
  (define-values (link-mapping
                  import-syms
                  import-sigs
                  export-sigs
                  cu-exprs)
    (parse-compound-unit form))
  
  (define (lookup-link-id id) (dict-ref link-mapping id #f))
  (define-values (check _ init-depends)
    (for/fold ([check -Void]
               [seen-init-depends import-syms]
               [calculated-init-depends '()])
              ([form (in-list cu-exprs)])
      (match-define (cu-expr-info unit-expr-stx
                                  import-sigs
                                  import-links
                                  export-sigs
                                  export-links)
        form)
      (define unit-expected-type 
        (-unit import-sigs 
               export-sigs 
               (map lookup-link-id (set-intersect seen-init-depends import-links))
               ManyUniv))
      (define unit-expr-type (tc-expr/t unit-expr-stx))
      (check-below unit-expr-type unit-expected-type)
      (define-values (body-type new-init-depends)
        (match unit-expr-type
          [(Unit: _ _ ini-deps ty)
           ;; init-depends here are strictly subsets of the units imports
           ;; but these may not exactly match with the provided links
           ;; so all of the extended signatures must be traversed to find the right
           ;; signatures for init-depends
           (define extended-imports
             (map cons import-links
                  (map (λ (l) (map Signature-name (flatten-sigs l))) import-sigs)))
           (define init-depend-links
             (for*/list ([sig-name (in-list (map Signature-name ini-deps))]
                         [(import-link import-family) (in-dict extended-imports)]
                         #:when (member sig-name import-family free-identifier=?))
               import-link))
           ;; new init-depends are the init-depends of this unit that
           ;; overlap with the imports to the compound-unit
           (values ty (set-intersect import-syms init-depend-links))]
          ;; unit-expr was not actually a unit, but we want to delay the errors
          [_ (values #f '())]))
      (values body-type
              ;; Add the exports to the list of seen-init-depends
              (set-union seen-init-depends export-links)
              ;; Add the new-init-depends to those already calculated
              (set-union calculated-init-depends new-init-depends))))
  (if check
      (-unit import-sigs
             export-sigs
             (map lookup-link-id init-depends)
             check)
      ;; Error case when one of the links was not a unit
      -Bottom))

(define (parse-and-check-compound/infer form expected-type)
  (define init-depend-refs (syntax-property form 'unit:inferred-init-depends))
  (syntax-parse form
    [cu:compound-unit-expansion
     (define unit-exprs (attribute cu.unit-exprs))
     (define compound-imports (map lookup-signature/check (attribute cu.compound-imports)))
     (define compound-exports (map lookup-signature/check (attribute cu.compound-exports)))
     (define import-vector (apply vector compound-imports))
     (define import-length (vector-length import-vector))
     (unless (and (list? init-depend-refs)
                  (andmap (λ (i) (and (exact-nonnegative-integer? i) (< i import-length)))
                          init-depend-refs))
       (int-err "malformed syntax property attached to compound-unit/infer form"))
     (define compound-init-depends
       (map (lambda (i) (vector-ref import-vector i)) init-depend-refs))
     (define resulting-unit-expr (last unit-exprs))
     (define final-unit-invoke-type (tc-expr/t resulting-unit-expr))
     ;; This type should always be a unit
     (match-define (Unit: _ _ _ compound-invoke-type) final-unit-invoke-type)
     (-unit compound-imports compound-exports compound-init-depends compound-invoke-type)]))

(define (parse-and-check-invoke form expected-type)
  (syntax-parse form 
    [iu:invoke-unit-expansion
     (define infer? (eq? 'infer (tr:unit:invoke-property form)))
     (define invoked-unit (attribute iu.expr))
     (define import-sigs (map lookup-signature/check (attribute iu.imports)))
     (define linking-units (attribute iu.units))
     (define unit-expr-type (tc-expr/t invoked-unit))
     ;; TODO: Better error message/handling when the folling check-below "fails"
     (unless infer?
       (check-below unit-expr-type (-unit import-sigs null import-sigs ManyUniv)))
     (for ([unit (in-list linking-units)]
           [sig (in-list import-sigs)])
       (define ids (extract-definitions unit))
       (define types (map cdr (Signature-mapping sig)))
       (for ([type (in-list types)]
             [id (in-list ids)])
         (define lexical-type (lookup-type/lexical id))
         (unless (subtype lexical-type type)
           (tc-error/fields "type mismatch in invoke-unit import"
                            "expected" type
                            "given" lexical-type
                            "imported variable" (syntax-e id)
                            "imported signature" (syntax-e (Signature-name sig))
                            #:stx form
                            #:delayed? #t))))
     (cond
       [(Unit? unit-expr-type)
        (define result-type (Unit-result unit-expr-type))
        (match result-type
          [(Values: (list (Result: t _ _) ...)) t]
          [(AnyValues: f) ManyUniv]
          [(ValuesDots: (list (Result: t _ _) ...) _ _) t])]
       [else -Bottom])]))

;; Parse and check unit syntax
(define (parse-and-check-unit form expected)
  (syntax-parse form
    [u:unit-expansion
     ;; extract the unit body syntax
     (define body-stx #'u.body-stx)
     (define import-sigs (attribute u.import-sigs))
     (define import-internal-ids (attribute u.import-internal-ids))
     (define import-tags (attribute u.import-sig-tags))
     (define export-sigs (attribute u.export-sigs))
     (define export-temp-ids (attribute u.export-temp-ids))
     (define init-depend-tags (attribute u.init-depend-tags))
     (define export-temp-internal-map
       (make-immutable-free-id-table
        (trawl-for-property body-stx export-map-elem? extract-export-map-elem)))
     (define-values (imports-info exports-info init-depends)
       (process-unit-syntax import-sigs import-internal-ids import-tags
                            export-sigs export-temp-ids export-temp-internal-map
                            init-depend-tags))

     ;; Get Signatures to build Unit type
     (define import-signatures (map lookup-signature/check (map sig-info-name imports-info)))
     (define export-signatures (map lookup-signature/check (map sig-info-name exports-info)))
     (define init-depend-signatures (map lookup-signature/check init-depends))

     (unless (distinct-signatures? import-signatures)
       (tc-error/expr "unit expressions must import distinct signatures"))
     ;; this check for exports may be unnecessary
     ;; the unit macro seems to check it as well
     (unless (distinct-signatures? export-signatures)
       (tc-error/expr "unit expresssions must export distinct signatures"))
     
     (define local-sig-type-map
       (apply append (map make-local-type-mapping imports-info)))
     (define export-signature-type-map
       (map (lambda (si)
              (cons (sig-info-name si) (make-local-type-mapping si)))
            exports-info))

     ;; Thunk to pass to tc/letrec-values to check export subtyping
     ;; These subtype checks can only be checked within the dynamic extent
     ;; of the call to tc/letrec-values because they need to lookup
     ;; variables in the type environment as modified by typechecking
     (define (check-exports-thunk)
       (for* ([sig-mapping (in-list export-signature-type-map)]
              [sig (in-value (car sig-mapping))]
              [mapping (in-value (cdr sig-mapping))]
              [(id expected-type) (in-dict mapping)])
         (define id-lexical-type (lookup-type/lexical id))
         (unless (subtype id-lexical-type expected-type)
           (tc-error/fields "type mismatch in unit export"
                            "expected" expected-type
                            "given" id-lexical-type
                            "exported variable" (syntax-e id)
                            "exported signature" (syntax-e sig)
                            #:delayed? #t))))
     
     (define import-name-map
       (append-map (lambda (si) (map cons (sig-info-externals si) (sig-info-internals si)))
                   imports-info))
     (define export-name-map
       (append-map (lambda (si) (map cons (sig-info-externals si) (sig-info-internals si))) 
                   exports-info))
     
     (define body-forms 
       (trawl-for-property body-stx tr:unit:body-exp-def-type-property))
     
     (define last-form 
       (or (and (not (empty? body-forms)) (last body-forms))))
     
     ;; get expression forms, if the body was empty or ended with
     ;; a definition insert a `(void)` expression to be typechecked
     ;; This is necessary because we defer to tc/letrec-values for typechecking
     ;; unit bodies, but a unit body may contain only definitions whereas letrec bodies
     ;; cannot, in this case we insert dummy syntax representing a call to the void 
     ;; function in order to correctly type the body of the unit.
     (define expression-forms
       (let ([exprs 
              (filter
               (lambda (stx) (eq? (tr:unit:body-exp-def-type-property stx) 'expr))
               body-forms)])
         (cond
           [(or (not last-form) (eq? (tr:unit:body-exp-def-type-property last-form) 'def/type))
            (append exprs (list #'(#%plain-app void)))]
           [else exprs])))

     
     
     ;; Filter out the annotation and definition syntax from the unit body
     ;; For the purposes of typechecking, annotations and definitions
     ;; are essentially lifted to the top of the body and all expressions
     ;; are placed at the end (possibly with the addition of a (void) expression
     ;; as described above), since the types of definitions and annotations
     ;; must scope over the entire body of the unit, this is valid for purposes
     ;; of typechecking
     (define annotation/definition-forms
       (filter
        (lambda (stx) (eq? (tr:unit:body-exp-def-type-property stx) 'def/type))
        body-forms))
     
     (define-values (ann/def-names ann/def-exprs)
       (process-ann/def-for-letrec annotation/definition-forms))

     (define signature-annotations
       (for/list ([(k v) (in-dict local-sig-type-map)])
         (cons k (-> v))))
     (define unit-type
       (with-lexical-env/extend-types
         (map car signature-annotations)
         (map cdr signature-annotations)
         ;; Typechecking a unit body is structurally similar to that of
         ;; checking a let-body, so we resuse the machinary for checking
         ;; let expressions
         (define res (tc/letrec-values ann/def-names
                                       ann/def-exprs
                                       (quasisyntax/loc form (#,@expression-forms))
                                       #f
                                       check-exports-thunk))
         (define invoke-type
           (match res
             [(tc-results: tps) (-values tps)]))
         (-unit import-signatures
                export-signatures
                init-depend-signatures
                invoke-type)))
     unit-type]))

;; Based on the function of the same name in check-class-unit.rkt
;; trawl-for-property : Syntax (Syntax -> Any) [(Syntax -> A)] -> (Listof A)
;; Search through the given syntax for pieces of syntax that satisfy
;; the accessor predicate, then apply the extractor function to  all such syntaxes 
(define (trawl-for-property form accessor [extractor values])
  (define (recur-on-all stx-list)
    (apply append (map (λ (stx) (trawl-for-property stx accessor extractor)) stx-list)))
  (syntax-parse form
    #:literal-sets (kernel-literals)
    [stx
     #:when (accessor #'stx)
     (list (extractor form))]
    [_
     (define list? (syntax->list form))
     (if list? (recur-on-all list?) '())]))
