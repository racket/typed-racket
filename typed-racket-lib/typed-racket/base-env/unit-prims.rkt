#lang racket/base

;; Primitive forms for units/signatures

(provide unit
         define-unit
         compound-unit 
         define-compound-unit
         compound-unit/infer
         define-compound-unit/infer
         invoke-unit
         invoke-unit/infer
         define-values/invoke-unit
         define-values/invoke-unit/infer
         unit-from-context
         define-unit-from-context)


(require  "../utils/utils.rkt"
          "colon.rkt"
          (for-syntax syntax/parse
                      racket/base
                      racket/list
                      racket/match
                      racket/syntax
                      racket/sequence
                      syntax/context
                      syntax/flatten-begin
                      syntax/kerncase
                      "../private/syntax-properties.rkt"
                      (typecheck internal-forms)
                      syntax/id-table
                      racket/dict
                      racket/unit-exptime
                      syntax/strip-context
                      (utils tc-utils)
                      syntax/id-table
                      syntax/id-set)
          (prefix-in untyped- (only-in racket/unit
                                       define-signature
                                       unit
                                       invoke-unit
                                       invoke-unit/infer
                                       compound-unit
                                       define-unit
                                       define-compound-unit
                                       define-values/invoke-unit
                                       define-values/invoke-unit/infer
                                       compound-unit/infer
                                       define-compound-unit/infer
                                       unit-from-context
                                       define-unit-from-context))
          (only-in racket/unit
                   extends
                   import
                   export
                   init-depend
                   link
                   prefix
                   rename)
          "base-types.rkt"
          "base-types-extra.rkt"
          (for-label "colon.rkt")
          (for-template (rep type-rep))
          (submod "../typecheck/internal-forms.rkt" forms))

(begin-for-syntax
  (define-literal-set colon #:for-label (:))

  ;; process-definition-form handles all of the `define-` style unit macros
  ;; such as define-unit, define-compound-unit, define-unit-from-context. but
  ;; not the corresponding unit, compound-unit, etc forms
  ;; Performs local expansion in order to apply a syntax property to the
  ;; subexpression of the `define-*` form correpsonding to the body of the
  ;; definition being created
  ;; - eg for a define-unit form, a syntax property will be attached to the
  ;;   subexpression that creates the unit
  (define (process-definition-form apply-property stx)
    (define exp-stx (local-expand stx (syntax-local-context) (kernel-form-identifier-list)))
    (syntax-parse exp-stx
      #:literal-sets (kernel-literals)
      [(begin e ...)
       (quasisyntax/loc stx
         (begin #,@(map (λ (e) (process-definition-form apply-property e))
                        (syntax->list #'(e ...)))))]
      [(define-values (name ...) rhs)
       (quasisyntax/loc stx (define-values (name ...) #,(ignore (apply-property #'rhs))))]
      ;; define-syntaxes that actually create the binding given in the
      ;; `define-*` macro will fall through to this case, and should be left as-is
      [_ exp-stx]))


  (define-splicing-syntax-class init-depend-form
    #:literals (init-depend)
    (pattern (~and this-syntax (init-depend sig:id ...))
             #:attr form (list #'this-syntax)
             #:with names #'(sig ...))
    (pattern (~seq)
             #:attr form '()
             #:with names #'()))
  
  ;; The `rename` attribute in the sig-spec syntax class is used to correctly
  ;; map names of signature bound variables in unit bodies to their names in
  ;; the fully expanded syntax. It applies prefixes and renamings from
  ;; signature specifications to identifiers.
  (define-syntax-class sig-spec
    #:literals (prefix rename)
    (pattern sig-id:id
             #:attr rename (lambda (id) id)
             #:with sig-name #'sig-id)
    (pattern (prefix p:id sig:sig-spec)
             #:attr rename (lambda (id) (format-id #'sig.sig-name
                                                 "~a~a"
                                                 #'p
                                                 ((attribute sig.rename) id)))
             #:with sig-name #'sig.sig-name)
    (pattern (rename sig:sig-spec (new:id old:id) ...)
             #:attr rename 
             (lambda (id)
               (define (lookup id)
                 (for/first ([old-id (in-syntax #'(old ...))]
                             [new-id (in-syntax #'(new ...))]
                             #:when (free-identifier=? id old-id))
                   new-id))
               (define rn ((attribute sig.rename) id))
               (or (lookup rn) rn))
             #:with sig-name #'sig.sig-name)))


;; imports/members : identifier? -> syntax?
;; given an identifier bound to a signature
;; returns syntax containing the signature name and the names of each variable contained
;; in the signature, this is needed to typecheck define-values/invoke-unit forms
(define-for-syntax (imports/members sig-id)
  (define-values (_1 imp-mem _2 _3) (signature-members sig-id sig-id))
  #`(#,sig-id #,@(map (lambda (id)
                        (local-expand
                         id
                         (syntax-local-context)
                         (kernel-form-identifier-list)))
                      imp-mem)))

;; Given a list of signature specs
;; Processes each signature spec to determine the variables exported
;; and produces syntax containing the signature id and the exported variables
(define-for-syntax (process-dv-exports es)
  (for/list ([e (in-list es)])
    (syntax-parse e
      [s:sig-spec
       (define sig-id #'s.sig-name)
       (define renamer (attribute s.rename))
       (define-values (_1 ex-mem _2 _3) (signature-members sig-id sig-id))
       #`(#,sig-id #,@(map renamer ex-mem))])))

;; Typed macro for define-values/invoke-unit
;; This has to be handled specially because the types of
;; the defined values must be registered in the environment
(define-syntax (define-values/invoke-unit stx)
  (syntax-parse stx 
    #:literals (import export)
    [(_ unit-expr
        (import isig:sig-spec ...)
        (export esig:sig-spec ...))
     (define imports-stx (syntax->list #'(isig.sig-name ...)))
     (define exports-stx (syntax->list #'(esig ...)))
     (define/with-syntax temp (syntax-local-introduce (generate-temporary)))
     #`(begin
         #,(internal (quasisyntax/loc stx
                       (define-values/invoke-unit-internal
                         (#,@(map imports/members imports-stx))
                         (#,@(process-dv-exports exports-stx)))))
         (: temp (Unit (import isig.sig-name ...)
                       (export esig.sig-name ...)
                       (init-depend isig.sig-name ...)
                       AnyValues))
         (define temp unit-expr)
         #,(ignore (quasisyntax/loc stx
                     (untyped-define-values/invoke-unit unit-expr
                                                        (import isig ...)
                                                        (export esig ...)))))]))
(begin-for-syntax
  ;; flat signatures allow easy comparisons of whether one
  ;; such flat-signature implements another
  ;; - name is the identifier corresponding to the signatures name
  ;; - implements is a free-id-set of this signature and all its ancestors
  ;; flat-signatures enable a comparison of whether one signature implements
  ;; another as a subset comparison on their contained sets of parent signatures
  (struct flat-signature (name implements) #:transparent)

  ;; implements? : flat-signature? flat-signature? -> Boolean
  ;; true iff signature sig1 implements signature sig2
  (define (implements? sig1 sig2)
    (match* (sig1 sig2)
      [((flat-signature name1 impls1) (flat-signature name2 impls2))
       (free-id-subset? impls2 impls1)]))

  ;; Given: a list of identifiers bound to static unit information
  ;; Returns: two lists
  ;; 1. A list of flat-signatures representing the signatures imported by
  ;;    the given units
  ;; 2. A list of flat-signatures representing the signatures exported by
  ;;    the given units
  (define (get-imports/exports unit-ids)
    (define-values (imports exports) 
      (for/fold ([imports null]
                 [exports null])
                ([unit-id (in-list unit-ids)])
        (match-define-values ((list (cons _ new-imports) ...)
                              (list (cons _ new-exports) ...))
                             (unit-static-signatures unit-id unit-id))
        (values (append imports new-imports) (append exports new-exports))))
    (values (map make-flat-signature imports)
            (map make-flat-signature exports)))

  ;; Given the id of a signature, return a corresponding flat-signature
  (define (make-flat-signature sig-name)
    (flat-signature sig-name (get-signature-ancestors sig-name)))

  ;; Walk the chain of parent signatures to build a list, and convert it to
  ;; a free-id-set
  (define (get-signature-ancestors sig)
    (immutable-free-id-set
     (with-handlers ([exn:fail:syntax? (λ (e) null)])
       (let loop ([sig sig] [ancestors null])
         (define-values (parent _1 _2 _3) (signature-members sig sig)) 
         (if parent
             (loop parent (cons sig ancestors))
             (cons sig ancestors))))))
  
  ;; Calculate the set of inferred imports for a list of units
  ;; The inferred imports are those which are not provided as
  ;; exports from any of the units taking signature subtyping into account
  (define (infer-imports unit-ids)
    (define-values (imports exports) (get-imports/exports unit-ids))
    (define remaining-imports (remove* exports imports implements?))
    (map flat-signature-name remaining-imports))

  ;; infer-exports returns all the exports from linked
  ;; units rather than just those that are not also
  ;; imported
  (define (infer-exports unit-ids)
    (define-values (imports exports) (get-imports/exports unit-ids))
    (map flat-signature-name exports))

  (define-splicing-syntax-class maybe-exports
    #:literals (export)
    (pattern (~seq)
             #:attr exports #f)
    (pattern (export sig:id ...)
             #:attr exports (syntax->list #'(sig ...))))
  
  (define-syntax-class dviu/infer-unit-spec
    #:literals (link)
    (pattern unit-id:id
             #:attr unit-ids (list #'unit-id))
    (pattern (link uid-inits:id ...)
             #:attr unit-ids (syntax->list #'(uid-inits ...)))))

;; Note: This may not correctly handle all use cases of
;; define-values/invoke-unit/infer
;; inferred imports and exports are handled in the following way
;; - the exports of ALL units being linked are added to the export list
;;   to be registered in tc-toplevel, this appears to be how exports are treated
;;   by the unit inference process
;; - inferred imports are those imports which are not provided by
;;   any of the exports
;; This seems to correctly handle both recursive and non-recursive
;; linking patterns
(define-syntax (define-values/invoke-unit/infer stx)
  (syntax-parse stx
    [(_ exports:maybe-exports us:dviu/infer-unit-spec)
     (define inferred-imports (infer-imports (attribute us.unit-ids)))
     (define inferred-exports (or (attribute exports.exports)
                                  (infer-exports (attribute us.unit-ids))))
     #`(begin
         #,(internal (quasisyntax/loc stx
                       (define-values/invoke-unit-internal
                         (#,@(map imports/members inferred-imports))
                         (#,@(process-dv-exports inferred-exports)))))
         #,(ignore
            (quasisyntax/loc stx (untyped-define-values/invoke-unit/infer #,@#'exports us))))]))

(define-syntax (invoke-unit/infer stx)
  (syntax-parse stx 
    [(_ . rest)
     (ignore
      (tr:unit:invoke-property
       (quasisyntax/loc stx (untyped-invoke-unit/infer . rest)) 'infer))]))

;; The typed invoke-unit macro must attach a syntax property to the expression
;; being invoked in order to reliably find it during typechecking.
;; Otherwise the expanded syntax may be confused for that of invoke-unit/infer
;; and be typechecked incorrectly
(define-syntax (invoke-unit stx)
  (syntax-parse stx
    [(invoke-unit expr . rest)
     (ignore
      (tr:unit:invoke-property
       (quasisyntax/loc stx
         (untyped-invoke-unit
          #,(tr:unit:invoke:expr-property #'expr #t)
          . rest)) #t))]))

;; Trampolining macro that cooperates with the unit macro in order
;; to add information needed for typechecking units
;; Essentially head expands each expression in the body of a unit
;;  - leaves define-syntaxes forms alone, to allow for macro definitions in unit bodies
;;  - Inserts syntax into define-values forms that allow mapping the names of definitions
;;    to their bodies during type checking
;;  - Also specially handles type annotations in order to correctly associate variables
;;    with their types
;;  - All other expressions are marked as 'expr for typechecking
(define-syntax (add-tags stx)
  (syntax-parse stx
    [(_ e)
     (define exp-e (local-expand #'e (syntax-local-context) (kernel-form-identifier-list)))
     (syntax-parse exp-e
       #:literals (begin define-values define-syntaxes :)
       [(begin b ...)
        #'(add-tags b ...)]
       [(define-syntaxes (name:id ...) rhs:expr)
        exp-e]
       ;; Annotations must be handled specially
       ;; Exported variables are renamed internally in units, which leads
       ;; to them not being correctly associated with their type annotations
       ;; This extra bit of inserted syntax allows the typechecker to
       ;; properly associate all annotated variables with their types.
       ;; The inserted lambda expression will be expanded to the internal
       ;; name of the variable being annotated, this internal name
       ;; can then be associated with the type annotation during typechecking
       [(define-values () (colon-helper (: name:id type) rest ...))
        (quasisyntax/loc stx
          (define-values ()
            #,(tr:unit:body-exp-def-type-property
               #`(#%expression
                  (begin (void (lambda () name))
                         (colon-helper (: name type) rest ...)))
               'def/type)))]
       [(define-values (name:id ...) rhs)
        (quasisyntax/loc stx
          (define-values (name ...)
            #,(tr:unit:body-exp-def-type-property
               #'(#%expression
                  (begin
                    (void (lambda () name ... (void)))
                    rhs))
               'def/type)))]
       [_
        (tr:unit:body-exp-def-type-property exp-e 'expr)])]
    [(_ e ...)
     #'(begin (add-tags e) ...)]))

(define-syntax (unit stx)
  (syntax-parse stx
    #:literals (import export)
    [(unit imports exports init-depends:init-depend-form e ...)
     (ignore
      (tr:unit
       (quasisyntax/loc stx
         (untyped-unit
          imports
          exports
          #,@(attribute init-depends.form)
          (add-tags e ...)))))]))

(define-syntax (define-unit stx)
  (syntax-parse stx
    #:literals (import export)
    [(define-unit uid:id
       imports
       exports
       init-depends:init-depend-form
       e ...)
     (process-definition-form
      (λ (stx) (tr:unit stx))
      (quasisyntax/loc stx
        (untyped-define-unit uid
          imports
          exports
          #,@(attribute init-depends.form)
          (add-tags e ...))))]))

(begin-for-syntax
  (define-syntax-class compound-imports
    #:literals (import)
    (pattern (import lb:link-binding ...)
             #:attr import-tags (syntax->list #'(lb.link-id ...))))
  (define-syntax-class compound-links
    #:literals (link)
    (pattern (link ld:linkage-decl ...)
             #:attr all-export-links (map syntax->list (syntax->list #'(ld.exported-keys ...)))
             #:attr all-import-links (map syntax->list (syntax->list #'(ld.imported-keys ...)))))
  (define-syntax-class linkage-decl
    (pattern ((lb:link-binding ...)
              unit-expr:expr
              link-id:id ...)
             #:attr exported-keys #'(lb.link-id ...)
             #:with imported-keys #'(link-id ...)))
  (define-syntax-class link-binding
    (pattern (link-id:id : sig-id:id)))

  ;; build-compound-unit-prop : (listof id) (listof (listof id?)) (listof id?)
  ;;                         -> (list (listof symbol?) 
  ;;                                  (listof (listof symbol?))
  ;;                                  (listof (listof symbol?)))
  ;; Process the link bindings of a compound-unit form
  ;; to return a syntax property used for typechecking compound-unit forms
  ;; The return value is a list to be attached as a syntax property to compound-unit
  ;; forms.
  ;; The list contains 3 elements
  ;; - The first element is a list of symbols corresponding to the link-ids of
  ;;   the compound-unit's imports
  ;; - The second element is a list of lists of symbols, corresponding to the
  ;;   link-ids exported by units in the compound-unit's linking clause
  ;; - The last element is also a list of lists of symbols, corresponding to the
  ;;   link-ids being imported by units in the compound-unit's linking clause
  (define (build-compound-unit-prop import-tags all-import-links all-export-links)
    (define table
      (make-immutable-free-id-table
       (for/list ([link (in-list (append import-tags (flatten all-export-links)))])
         (cons link (gensym (syntax-e link))))))
    (define imports-tags
      (map (λ (id) (free-id-table-ref table id #f)) import-tags))
    (define units-exports
      (map
       (λ (lst) (map (λ (id) (free-id-table-ref table id #f)) lst))
       all-export-links))
    (define units-imports
      (for/list ([unit-links (in-list all-import-links)])
        (for/list ([unit-link (in-list unit-links)])
          (free-id-table-ref table unit-link #f))))
    (list imports-tags units-exports units-imports)))

(define-syntax (compound-unit stx)
  (syntax-parse stx
    [(_ imports:compound-imports
        exports
        links:compound-links)
     (define import-tags (attribute imports.import-tags))
     (define all-import-links (attribute links.all-import-links))
     (define all-export-links (attribute links.all-export-links))
     (define prop (build-compound-unit-prop import-tags all-import-links all-export-links))
     (ignore (tr:unit:compound-property
              (quasisyntax/loc stx (untyped-compound-unit imports exports links))
              prop))]))

(define-syntax (define-compound-unit stx)
  (syntax-parse stx
    [(_ uid 
        imports:compound-imports
        exports
        links:compound-links)
     (define import-tags (attribute imports.import-tags))
     (define all-import-links (attribute links.all-import-links))
     (define all-export-links (attribute links.all-export-links))
     (define prop (build-compound-unit-prop import-tags all-import-links all-export-links))
     (process-definition-form
      (λ (stx) (tr:unit:compound-property stx prop))
      (quasisyntax/loc stx
        (untyped-define-compound-unit uid imports exports links)))]))

(define-syntax (compound-unit/infer stx)
  (syntax-parse stx
    #:literals (import export link)
    [(_ . rest)
     (ignore
      (tr:unit:compound-property
       (quasisyntax/loc stx
         (untyped-compound-unit/infer . rest))
       'infer))]))

(define-syntax (define-compound-unit/infer stx)
  (syntax-parse stx
    [(_ . rest)
     (process-definition-form 
      (λ (stx) (tr:unit:compound-property stx'infer))
      (quasisyntax/loc stx (untyped-define-compound-unit/infer . rest)))]))

(define-syntax (unit-from-context stx)
  (syntax-parse stx
    [(_ . rest)
     (ignore
      (tr:unit:from-context
       (quasisyntax/loc stx
         (untyped-unit-from-context . rest))))]))

(define-syntax (define-unit-from-context stx)
  (syntax-parse stx
    [(_ . rest)
     (process-definition-form
      (λ (stx) (tr:unit:from-context stx))
      (quasisyntax/loc stx (untyped-define-unit-from-context . rest)))]))
