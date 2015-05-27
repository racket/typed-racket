#lang racket/unit

;; This module provides a unit for type-checking units

(require "../utils/utils.rkt"
         racket/dict
         racket/format
         racket/list
         racket/match
         racket/set
         racket/syntax
         syntax/id-table
         syntax/parse
         syntax/stx
         syntax/strip-context
         racket/unit-exptime
         "signatures.rkt"
         (private parse-type syntax-properties type-annotation)
         (env lexical-env tvar-env global-env 
              signature-env signature-helper)
         (types utils abbrev union subtype resolve generalize)
         (typecheck check-below internal-forms)
         (utils tc-utils)
         (rep type-rep)
         (for-syntax racket/base racket/unit-exptime)
         (for-template racket/base
                       (private unit-literals)
                       (submod "internal-forms.rkt" forms)))

(import tc-if^ tc-lambda^ tc-app^ tc-let^ tc-expr^)
(export check-unit^)

;; id-set-diff : (listof id) (listof id) -> (listof id)
;; returns the set difference of two lists of identifers
(define (id-set-diff s1 s2)
    (filter
     (lambda (id) (not (member id s2 free-identifier=?)))
     s1))

;; Syntax class deifnitions
;; variable annotations expand slightly differently in the body of a unit
;; due to how the unit macro transforms internal definitions
(define-syntax-class unit-body-annotation
  #:literal-sets (kernel-literals)
  #:literals (void values :-internal cons)
  (pattern
   (#%expression
    (begin 
      (#%plain-app void)
      (begin
        (quote
         (:-internal var:id t))
        (#%plain-app values))))
   #:attr name #'var
   ;; Needed to substiture the internal renaming of exported definitions
   ;; in unit bodies in order to ensure that the annotation is applied
   ;; to the correct identifier
   #:attr letrec-form #`(begin
                          (quote
                           (:-internal var t))
                          (#%plain-app values))
   #:attr subst-annotation (lambda (id)
                             #`(begin
                                 (quote
                                  (:-internal #,id t))
                                 (#%plain-app values)))))

(define-syntax-class unit-body-definition
  #:literal-sets (kernel-literals)
  #:literals (void)
  (pattern
   (#%expression
    (begin
      (#%plain-app void (#%plain-lambda () var:id) ...)
      e))
   #:with vars #'(var ...)
   #:with body #'e
   #:attr letrec-form #'e))


(define (process-ann/def-for-letrec ann/defs export-mapping import-mapping)
  (define import-names (map car import-mapping))
  (for/fold ([names #`()]
             [exprs #`()]
             [unannotated-exports (map car export-mapping)])
            ([a/d (in-list ann/defs)])
    (syntax-parse a/d
      [a:unit-body-annotation
       (define name (attribute a.name))
       ;; TODO:
       ;; Duplicate annotations from imports
       ;; are not currently detected due to a bug
       ;; in tc/letrec-values
       (define lookup-result (lookup-type name export-mapping))
       (define form ((attribute a.subst-annotation) 
                     (or lookup-result name)))
       (define new-unannotated-exports (remove name unannotated-exports))
       (values #`(#,@names ())
               #`(#,@exprs #,form)
               new-unannotated-exports)]
      [d:unit-body-definition
       (values #`(#,@names d.vars) #`(#,@exprs d.body) unannotated-exports)])))



(struct sig-info (name externals internals) #:transparent)
;; A Sig-Info is a (sig-info identifier? (listof identifier?) (listof identifier?))
;; name is the identifier corresponding to the signature this sig-info represents
;; externals is the list of external names for variables in the signature
;; internals is the list of internal names for variables in the signature

(define-syntax-class unit-int-rep
  #:literal-sets (kernel-literals)
  (pattern (#%plain-app int-var:id)
           #:with id #'int-var)
  (pattern int-var:id
           #:with id #'int-var))

(define-syntax-class index-row
  #:literal-sets (kernel-literals)
  #:literals (list cons)
  (pattern 
   (#%plain-app list (quote sig-id:id) 
                (#%plain-app cons (quote var-ext:id)
                             (#%plain-lambda () var-int:unit-int-rep)) ...)
   
   #:with name #'sig-id
   #:attr info (sig-info #'sig-id 
                         (syntax->list #'(var-ext ...)) 
                         (syntax->list #'(var-int.id ...)))))

(define (get-info stx)
  (syntax-parse stx
    [ir:index-row
     (attribute ir.info)]))

(define-syntax-class signature-index-table
  #:literal-sets (kernel-literals)
  #:literals (void list cons values)
  (pattern
   (let-values ([() (#%expression
                     (begin (#%plain-app void import:index-row ...)
                            (#%plain-app values)))]
                [() (#%expression
                     (begin (#%plain-app void export:index-row ...)
                            (#%plain-app values)))]
                [() (#%expression
                     (begin (#%plain-app void (quote init-depend:id) ...)
                            (#%plain-app values)))])
     (#%plain-app void))
   #:attr imports (map get-info (syntax->list #'(import ...)))
   #:attr exports (map get-info (syntax->list #'(export ...)))
   #:attr init-depends (syntax->list #'(init-depend ...))))

(define (parse-index-table stx)
  (syntax-parse stx
    [t:signature-index-table
     (values (attribute t.imports)
             (attribute t.exports)
             (attribute t.init-depends))]))

(define-syntax-class unit-expansion
  #:literals (let-values letrec-syntaxes+values #%plain-app quote)
  #:attributes (;imports
                ;exports
                ;init-depends
                body-stx)
  (pattern (#%plain-app
            make-unit:id
            name:expr 
            import-vector:expr
            export-vector:expr
            list-dep:expr
            unit-body:expr)
           #:with body-stx #'unit-body))


;; Compound Unit syntax classes
(define-syntax-class compound-unit-expansion
  #:literals (#%expression #%plain-app void quote-syntax begin)
  (pattern (#%expression
            (begin
              (#%plain-app void (quote-syntax link-id) ...)
              (#%plain-app void (quote-syntax sig-id) ...)
              (#%plain-app void (quote-syntax import-link) ...)
              (#%plain-app void (quote-syntax export-link) ...)
              infer-table
              untyped-compound-unit-exp:expr))
           #:attr link-id-mapping (map cons 
                                    (syntax->list #'(link-id ...))
                                    (syntax->list #'(sig-id ...)))
           #:attr import-link-ids (syntax->list #'(import-link ...))
           #:attr export-link-ids (syntax->list #'(export-link ...))
           #:with compound-unit #'untyped-compound-unit-exp))

(define-syntax-class compound-unit-expr
  #:literals (#%expression #%plain-app void quote-syntax begin)
  (pattern (#%expression
            (begin
              (#%plain-app void (quote-syntax export-sig:id) ...)
              (#%plain-app void (quote-syntax export-link:id) ...)
              (#%plain-app void (quote-syntax import-link:id) ...)
              unit-expr:expr))
           #:attr export-sigs (syntax->list #'(export-sig ...))
           #:attr export-links (syntax->list #'(export-link ...))
           #:attr import-links (syntax->list #'(import-link ...))
           #:with expr #'unit-expr))


;; infer-link
;; unit-id : the identifier corresponding to the unit being linked
;; export-links : the link ids being exported by the unit being linked
;; export-sigs : the identifiers corresponding to signatures exported by the unit
;; import-links : the identifiers of the links imported by the unit being linked
;; all-imports : all staticaly known imports of the unit being linked
;; all-exports : all statically known exports of the unit being linked
(struct infer-link (unit-id export-links export-sigs import-links all-imports all-exports) #:transparent)


(define (parse-infer-table-item stx)
  (syntax-parse stx
    #:literals (void quote-syntax #%plain-app begin #%expression #%plain-lambda)
    [(#%expression
      (begin
        (#%plain-app void (quote-syntax sig-ex:id) ...)
        (#%plain-app void (quote-syntax link-ex:id) ...)
        (#%plain-app void (quote-syntax link-im:id) ...)
        (#%plain-app void (quote-syntax import-sig:id) ...)
        (#%plain-app void (quote-syntax export-sig:id) ...)
        (#%plain-app void (quote-syntax (#%plain-app values unit-runtime-id:id)))))
     (define type-of-id (lookup-type/lexical #'unit-runtime-id))
     (infer-link #'unit-runtime-id
                 (syntax->list #'(link-ex ...))
                 (syntax->list #'(sig-ex ...))
                 (syntax->list #'(link-im ...))
                 (syntax->list #'(import-sig ...))
                 (syntax->list #'(export-sig ...)))]))

;; process-infer-table : Infer-Table (Listof (Cons Id Id)) -> (Listof Syntax) (Listof (id . id))
;; Process a compound-unit/infer forms infer table to produce syntax expected
;; for typechecking a compound-unit, also update the link mapping
;;
;; We assume that the imports/exports to be inserted are uniquely determined
;; because otherwise the untyped macro should have failed at compile time
;;
;; Algorithm:
;; 1. Parse the representation of the infer-link
;; 2. Perform a first pass to insert exports and update the link mapping
;; 3. A second pass finishes by adding inferred imports looked up in the
;;    new link mapping
(define (process-infer-table infer-table init-link-map)
  ;; Pass 1: parse infer-table items into infer-link structs
  (define infer-links (map parse-infer-table-item infer-table))
  
  ;; Pass 2: Use static information associated with the unit-id
  ;; to fill out all the export information
  (define-values (infer-links/exports link-map/exports)
    (for/fold ([infer-links/exports null]
               [link-map/exports init-link-map])
              ([link infer-links])
      (match-define (infer-link unit-id export-links export-sigs import-links imports exports)
                    link)
      (define new-exports (filter-not 
                           (lambda (id) (member id export-sigs free-identifier=?))
                           exports))
      (define new-links (generate-temporaries new-exports))
      (define link-map-extension (map cons new-links new-exports))
      (values
       ;; create an extended infer-link
       (append infer-links/exports
               (list
                (infer-link unit-id 
                            (append new-links export-links) 
                            (append new-exports export-sigs)
                            import-links
                            imports
                            exports)))       
       ;; update the link-map
       (append link-map-extension link-map/exports))))
  
  ;; Pass 3: Use static information to fill in the import link-ids
  (define sig/link-map (map
                        (match-lambda 
                         [(cons k v) (cons v k)])
                        link-map/exports))
  (define infer-links/imports/exports
    (for/fold ([infer-links/imports/exports null])
              ([link infer-links/exports])
      (match-define (infer-link unit-id ex-links ex-sigs import-links import-sigs exports) link)
      (define new-links
        (filter-not
         (lambda (id) (member id import-links free-identifier=?))
         (map (lambda (id) (lookup-type id sig/link-map)) import-sigs)))
      
      (append infer-links/imports/exports
              (list (infer-link unit-id ex-links 
                                ex-sigs (append new-links import-links)
                                import-sigs exports)))))
  
  ;; Finally map over the list converting the structs into syntax expected for typechecking
  (define forms-to-check
    (map
    (match-lambda
     [(infer-link unit-id ex-links ex-sigs im-links _ _)
      (define (ids->row ids)
        #`(#%plain-app void 
                       #,@(map (lambda (id) #`(quote-syntax #,id)) ids)))
      #`(#%expression
         (begin
           #,(ids->row ex-sigs)
           #,(ids->row ex-links)
           #,(ids->row im-links)
           #,unit-id))])
    infer-links/imports/exports))
  (values forms-to-check
          link-map/exports))


;; parse-compound-unit : Syntax -> (Values (Listof (Cons Id Id))
;;                                         (Listof Id)
;;                                         (Listof Id)
;;                                         Syntax)
;; Returns a mapping of link-ids to sig-ids, a list of imported sig ids
;; a list of exported link-ids
(define (parse-compound-unit stx)
  (syntax-parse stx
    [cu:compound-unit-expansion
     (define mapping (attribute cu.link-id-mapping))
     (define link-ids (map car mapping))
     (define export-signatures 
       (map (lambda (id) 
              (if (member id link-ids free-identifier=?)
                  (lookup-type id mapping)
                  id)) 
            (attribute cu.export-link-ids)))
     (define infer-table
       (syntax-parse #'cu.infer-table
         #:literals (#%plain-app void)
         [(#%plain-app void) #f]
         [e #'e]))
     (values 
      mapping
      (attribute cu.import-link-ids)
      export-signatures
      infer-table
      #'cu.compound-unit)]))

;; parse-compound-unit-expr : Syntax -> (Values Syntax (Listof Id) (Listof Id))
;; Given a unit expression form from ompound unit
;; Returns an expression to typecheck, a list of imported link-ids
;; and a list of exported sig-ids
(define (parse-compound-unit-expr stx)
  (syntax-parse stx
    [cue:compound-unit-expr
     (values
      #'cue.expr
      (attribute cue.import-links)
      (attribute cue.export-links)
      (attribute cue.export-sigs))]))


;; Sig-Info -> (listof (pairof identifier? Type))
;; GIVEN: signature information
;; RETURNS: a mapping from internal names to types
(define (make-local-type-mapping si)
  (define sig (lookup-signature (sig-info-name si)))
  (define internal-names (sig-info-internals si))
  (define sig-types 
    (map cdr (Signature-mapping sig)))
  (map cons internal-names sig-types))

(define (arrowize-mapping mapping)
  (for/list ([(k v) (in-dict mapping)])
    (cons k (-> v))))

;; combine this and the above function later
(define (make-external-type-mapping si)
  (define sig (sig-info-name si))
  (define external-names (sig-info-externals si))
  (define sig-types 
    (map cdr (signature->bindings sig)))
  (map cons external-names sig-types))

(define (lookup-type name mapping)
  (let ([v (assoc name mapping free-identifier=?)])
    (and v (cdr v))))


;; Syntax Option<TCResults> -> TCResults
;; Type-check a unit form
(define (check-unit form [expected #f])
  (define expected-type
    (match expected
      [(tc-result1: type) (resolve type)]
      [_ #f]))
  (match expected-type
    [(? Unit? unit-type)
     (ret (parse-and-check form unit-type))]
    [_ (ret (parse-and-check form #f))]))

;; Syntax Option<TCResultss> -> TCResults

(define (check-invoke-unit form [expected #f])
  (define expected-type 
    (match expected
      ;; I think this is wrong, since invoke may return multiple values
      [(tc-result1: type) (resolve type)]
      [_ #f]))
  (define ret-val (ret (parse-and-check-invoke form expected-type)))
  ret-val)

;; Handle checking compound-unit and compound-unit/infer forms
;; the following invariant should hold
;; if the infer-table is #f then the form was compound-unit and
;; the list of forms-to-check will be non-empty
;; otherwise the expression was compound-unit/infer 
;; and the forms-to-check list will be empty but infer-table will
;; contain the relevant forms
;;
;; In either case a pass over both of these expressions
;; will produce the correct list of forms to typecheck
(define (check-compound-unit form [expected #f])
  (define expected-type
    (match expected
      [(tc-result1: type) (resolve type)]
      [_ #f]))
  (ret (parse-and-check-compound form expected-type)))

(define (check-unit-from-context form [expected #f])
  (define expected-type
    (match expected
      [(tc-result1: type) (resolve type)]
      [_ #f]))
  (ret (parse-and-check-unit-from-context form expected-type)))

(define (parse-and-check-unit-from-context form expected-type)
  (syntax-parse form
    #:literals (#%expression begin void quote-syntax #%plain-app)
    [(#%expression
      (begin (#%plain-app void (quote-syntax sig-id:id)) unit))
     (define sig (lookup-signature #'sig-id))
     (define valid?
       (for/and ([(-id sig-type) (in-dict (Signature-mapping sig))])
         (define id (replace-context #'sig-id -id))
         (define lexical-type (lookup-type/lexical id))
         (if (subtype lexical-type sig-type)
             #t
             (begin (type-mismatch sig-type lexical-type) #f))))
     (if valid?
         (-unit null (list sig) null (-values (list -Void)))
         -Bottom)]))

(define (parse-and-check-compound form expected-type)
  (define-values (init-link-mapping compound-import-links compound-export-sigs infer-table compound-expr)
    (parse-compound-unit form))
  
  ;; Get forms to check, and extend the link mapping if bindings must be inferred
  (define-values (forms-to-check link-mapping)
    (if infer-table
        (process-infer-table 
         (trawl-for-property infer-table tr:unit:compound:expr-property)
         init-link-mapping)
        (values
         (trawl-for-property compound-expr tr:unit:compound:expr-property)
         init-link-mapping)))
  
  (define (lookup-link-id id) (lookup-type id link-mapping))
  (define (lookup-sig-id id) 
    (lookup-type id (map (lambda (k/v) (cons (cdr k/v) (car k/v))) link-mapping)))
  
  (define (set-union lst1 lst2)
    (remove-duplicates (append lst1 lst2) free-identifier=?))
  (define (set-intersect lst1 lst2)
    (filter 
     (lambda (e) (member e lst2 free-identifier=?))
     lst1))
  
  (define import-signatures (map lookup-signature (map lookup-link-id compound-import-links)))
  (define export-signatures (map lookup-signature compound-export-sigs))
  
  (define-values (check _ init-depends)
    (for/fold ([check -Void]
               [seen-init-depends compound-import-links]
               [calculated-init-depends '()])
              ([form (in-list forms-to-check)])
      (define-values (unit-expr-stx import-link-ids export-link-ids export-sig-ids)
        (parse-compound-unit-expr form))
      
      (define import-sigs (map lookup-signature (map lookup-link-id import-link-ids)))
      (define export-sigs (map lookup-signature export-sig-ids))
            
      (define unit-expected-type 
        (-unit import-sigs 
               export-sigs 
               (map lookup-signature
                    (map lookup-link-id (set-intersect seen-init-depends import-link-ids))) 
               ManyUniv))
      (define unit-expr-type (tc-expr/t unit-expr-stx))
      (check-below unit-expr-type unit-expected-type)
      (define-values (check new-init-depends)
        (match unit-expr-type
          [(Unit: _ _ ini-deps ty)
           (values ty (set-intersect (map Signature-name ini-deps) (map lookup-link-id compound-import-links)))]
          [_ (values #f null)]))
      (values check 
              (set-union seen-init-depends export-link-ids) 
              (set-union calculated-init-depends new-init-depends))))
  (if check
      (-unit import-signatures export-signatures (map lookup-signature init-depends) check)
      -Bottom))

;; syntax class for invoke-table
(define-syntax-class invoke-table
  #:literals (let-values void #%plain-app #%expression begin quote-syntax)
  (pattern (let-values ([()
                         (#%expression
                          (begin (#%plain-app void (quote-syntax sig-id:id) sig-var:id ...)
                                 (#%plain-app values)))]
                        ...)
             invoke-expr:expr)
           #:with sig-vars #'(sig-var ... ...)))

(define (parse-and-check-invoke form expected-type)
  (define-values (sig-ids unit-expr-stx infer?)
    (syntax-parse form
      #:literal-sets (kernel-literals)
      #:literals (void values)
      [(#%expression 
        (begin
          (#%plain-app void (~optional (quote-syntax (#%plain-app values infer-id:id)) 
                                       #:defaults ([infer-id #f])))
          (#%plain-app void (quote-syntax sig-id:id) ...)
          expr))
       (define unit-expr-stx
         (or (attribute infer-id)
             (first (trawl-for-property #'expr tr:unit:invoke:expr-property))))
       (values (syntax->list #'(sig-id ...)) unit-expr-stx (attribute infer-id))]))
  
  (define import-signatures (map lookup-signature sig-ids))
  (define imports-sig-stx (map cons import-signatures sig-ids))
  (define expected-unit-type
    ;; is null the correct value for the init-depend signatures???
    (-unit import-signatures null null ManyUniv))
  (define unit-expr-type
    (tc-expr/t unit-expr-stx))
  ;; only check subtyping when not using invoke-unit/infer
  (unless infer?
    (define subtype? (subtype unit-expr-type expected-unit-type))
    (unless subtype?
      (match unit-expr-type
        [(Unit: imports _ _ _)
         (define expected-import-sigs (map Signature-name imports))
         (define declared-import-sigs (map Signature-name import-signatures))
         (define missing-imports (id-set-diff expected-import-sigs declared-import-sigs))
         ;; missing-imports cannot be an empty list since subtype? is #f
         (tc-error/fields "insufficient imports to invoke-unit expression"
                          "expected imports" (map syntax-e expected-import-sigs)
                          "declared imports" (map syntax-e declared-import-sigs)
                          "missing imports" (map syntax-e missing-imports)
                          #:delayed? #t)]
        [_ (check-below unit-expr-type expected-unit-type)])))
  (cond 
    ;; not a unit then tc-error/expr
    [(Unit? unit-expr-type)
     (for* ([(sig stx) (in-dict imports-sig-stx)]
            ;; This is wrong need to flatten the mapping to get members from 
            ;; parent signatures as well
            [(-id sig-type) (in-dict (Signature-mapping sig))])
       ;; FIXME: so that replacing this context is unnecessary
       (define id (format-id stx "~a" -id))
       (define lexical-type (lookup-type/lexical id))
       ;; type mismatch 
       (unless (subtype lexical-type sig-type)
         (tc-error/fields "type mismatch in invoke-unit import"
                            "expected" sig-type
                            "given" lexical-type
                            "imported variable" (syntax-e id)
                            "imported signature" (syntax-e (Signature-name sig))
                            #:delayed? #t)))
     (define result-type (Unit-result unit-expr-type))
     (match result-type
       [(Values: (list (Result: t _ _) ...))
        t]
       [(AnyValues: f) ManyUniv]
       ;; Should there be a ValuesDots case here?
       )]
    [else -Bottom]))


(define (parse-and-check form expected)
  (syntax-parse form
    [u:unit-expansion
     (define body-stx #'u.body-stx)
     ;; handle signature table information
     (define unit-index-table 
       (first (trawl-for-property body-stx tr:unit:index-table-property)))
     (define-values (imports-info exports-info init-depends)
       (parse-index-table unit-index-table))
     
     ;; Get Signatures to build Unit type
     (define import-signatures (map lookup-signature (map sig-info-name imports-info)))
     (define export-signatures (map lookup-signature (map sig-info-name exports-info)))
     (define init-depend-signatures (map lookup-signature init-depends))
     
     (define local-sig-type-map
       (apply append (map make-local-type-mapping imports-info)))
     
     ;; Need to pass on to tc/letrec to ensure variables defined with the correct types
     (define export-signature-type-map
       (map (lambda (si)
              (cons (sig-info-name si) (make-local-type-mapping si)))
            exports-info))
     
     ;; Thunk to pass to tc/letrec-values to check export subtyping
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
       (apply append (map
                      (lambda (si) (map cons (sig-info-externals si) (sig-info-internals si)))
                      imports-info)))
     (define export-name-map
       (apply append (map 
                      (lambda (si) (map cons (sig-info-externals si) (sig-info-internals si))) 
                      exports-info)))
     
     ;; extract unit body forms
     (define body-forms 
       (trawl-for-property body-stx tr:unit:body-exp-def-type-property))
     
     ;; get last expression in the body
     (define last-form 
       (or (and (not (empty? body-forms)) (last body-forms))))
     
     ;; get expression forms, if the body was empty or ended with
     ;; a definition insert a `(void)` expression to be typechecked
     (define expression-forms
       (let ([exprs 
              (filter
               (lambda (stx) (eq? (tr:unit:body-exp-def-type-property stx) 'expr))
               body-forms)])
         (cond
          [(not last-form) (append exprs (list #'(#%plain-app void)))]
          [(eq? (tr:unit:body-exp-def-type-property last-form) 'def/type)
           (append exprs (list #'(#%plain-app void)))]
          [else exprs])))
     
     ;; get the definitions/annotations in the body
     (define  annotation/definition-forms
       (filter
        (lambda (stx) (eq? (tr:unit:body-exp-def-type-property stx) 'def/type))
        body-forms))
     
     (define-values (ann/def-names ann/def-exprs unannotated-exports)
       (process-ann/def-for-letrec annotation/definition-forms 
                                   export-name-map 
                                   import-name-map))

     ;; TODO: Export signature types should be introduced for typechecking
     ;;       the body of the unit if not already specified in the body
     ;;       This is more of a pragmatic feature than one required to typecheck
     ;;       but it would make porting programs somewhat simpler
     ;;       Currently adding these to the lexical scope doesn't seem to work, I think
     ;;       they need to be turned into real annotations that are inserted
     ;;       into the tc/letrec-values call
     
     (define signature-annotations (arrowize-mapping local-sig-type-map))
     (define unit-type
       (with-lexical-env/extend-types
         (map car signature-annotations)
         (map cdr signature-annotations)
         (define res (tc/letrec-values ann/def-names
                                       ann/def-exprs
                                       #`(#,@expression-forms)
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

;; extended version of the function in check-class-unit.rkt
;; Syntax (Syntax -> Any) -> Listof<Syntax>
;; Look through the expansion of the unit macro in search for
;; syntax with some property (e.g., definition bodies and expressions)
(define (trawl-for-property form accessor)
  (define (recur-on-all stx-list)
    (apply append (map (λ (stx) (trawl-for-property stx accessor))
                       (syntax->list stx-list))))
  (syntax-parse form
    #:literals (let-values letrec-values #%plain-app
                #%plain-lambda letrec-syntaxes+values
                #%expression begin)
    [stx
     #:when (accessor #'stx)
     (list form)]
    [(let-values ([(x ...) rhs ...] ...) body ...)
     (recur-on-all #'(rhs ... ... body ...))]
    ;; for letrecs, traverse the RHSs too
    [(letrec-values ([(x ...) rhs ...] ...) body ...)
     (recur-on-all #'(rhs ... ... body ...))]
    [(letrec-syntaxes+values (sb ...) ([(x ...) rhs ...] ...) body ...)
     (recur-on-all #'(rhs ... ... body ...))]
    [(#%plain-app e ...)
     (recur-on-all #'(e ...))]
    [(#%plain-lambda (x ...) e ...)
     (recur-on-all #'(e ...))]
    [(begin e ...)
     (recur-on-all #'(e ...))]
    [(#%expression e)
     (recur-on-all (if (syntax->list #'e) #'e #'()))]
    [(() e)
     (recur-on-all #'(e))]
    [_ '()]))
