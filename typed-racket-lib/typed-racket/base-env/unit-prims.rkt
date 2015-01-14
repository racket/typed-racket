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
          "../private/unit-literals.rkt"
          (for-syntax syntax/parse
                      racket/base
                      racket/list
                      racket/match
                      racket/syntax
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

  (define (process-definition-form apply-property stx)
    (define exp-stx (local-expand stx (syntax-local-context) (kernel-form-identifier-list)))
    (syntax-parse exp-stx
      #:literal-sets (kernel-literals)
      [(begin e ...)
       (quasisyntax/loc stx
         (begin #,@(map (λ (e) (process-definition-form apply-property e))
                        (syntax->list (syntax/loc stx (e ...))))))]
      [(define-values (name ...) rhs)
       (quasisyntax/loc stx (define-values (name ...) #,(ignore (apply-property #'rhs))))]
      [_ exp-stx]))


  (define-splicing-syntax-class init-depend-form
    #:literals (init-depend)
    (pattern (init-depend sig:id ...)
             #:attr form (list #'(init-depend sig ...))
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
               (define mapping  (map cons  
                                     (syntax->list #'(old ...))
                                     (syntax->list #'(new ...))))
               (define (lookup id) 
                 (define lu (member id mapping 
                                    (lambda (x y) 
                                      (free-identifier=? x (car y)))))
                 (and lu (cdar lu)))

               (define rn ((attribute sig.rename) id))
               (or (lookup rn) rn))
             #:with sig-name #'sig.sig-name)))


;; imports/members : identifier? -> syntax?
;; given an identifier representing a signature
;; returns syntax containing the signature name and the names of each variable contained
;; in the signature, this is needed to typecheck define-values/invoke-unit forms
(define-for-syntax (imports/members sig-id)
  (match-define-values (_ (list imp-mem ...) _ _)
                       (signature-members sig-id sig-id))
  #`(#,sig-id #,@(map (lambda (id)
                        (local-expand
                         id
                         (syntax-local-context)
                         (kernel-form-identifier-list)))
                      imp-mem)))

(define-for-syntax (process-dv-exports es)
  (for/list ([e (in-list es)])
    (syntax-parse e
      [s:sig-spec
       (define sig-id #'s.sig-name)
       (define renamer (attribute s.rename))
       (match-define-values (_ (list ex-mem ...) _ _)
                            (signature-members sig-id sig-id))
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
                       ;; FIXME this needs to be AnyValues
                        Any))
         (define temp unit-expr)
         #,(ignore (quasisyntax/loc stx
                     (untyped-define-values/invoke-unit unit-expr
                                                        (import isig ...)
                                                        (export esig ...)))))]))
(begin-for-syntax
  (define (get-imports/exports unit-ids)
    (for/fold ([imports null]
               [exports null])
              ([unit-id (in-list unit-ids)])
      (match-define-values ((list (cons _ new-imports) ...)
                            (list (cons _ new-exports) ...))
                           (unit-static-signatures unit-id unit-id))
      (values (append imports new-imports) (append exports new-exports))))
  
  (define (infer-imports unit-ids)
    (define-values (imports exports) (get-imports/exports unit-ids))
    (free-id-set->list (free-id-set-subtract (immutable-free-id-set imports)
                                             (immutable-free-id-set exports))))

  ;; infer-exports returns all the exports from linked
  ;; units rather than just those that are not also
  ;; imported
  (define (infer-exports unit-ids)
    (define-values (imports exports) (get-imports/exports unit-ids))
    exports)
  
  (define-syntax-class define/invoke/infer-form
    #:literals (define-values/invoke-unit/infer)
    (pattern (define-values/invoke-unit/infer 
               exports:maybe-exports
               us:unit-spec)
             #:with untyped-stx
             #`(untyped-define-values/invoke-unit/infer
                #,@#'exports us)
             #:attr inferred-imports
             (infer-imports (attribute us.unit-ids))
             #:attr inferred-exports
             (or (attribute exports.exports)
                 (infer-exports (attribute us.unit-ids)))))
  
  (define-splicing-syntax-class maybe-exports
    #:literals (export)
    (pattern (~seq)
             #:attr exports #f)
    (pattern (export sig:id ...)
             #:attr exports (syntax->list #'(sig ...))))
  
  (define-syntax-class unit-spec
    #:literals (link)
    (pattern unit-id:id
             #:attr unit-ids (syntax->list #'(unit-id)))
    (pattern (link uid-inits:id ...)
             #:attr unit-ids (syntax->list #'(uid-inits ...)))))

;; Note: This may not correctly handle all use cases of
;; define-values/invoke-unit/infer
;; inferred imports and exports are handled in the following way
;; - the exports of ALL units being linked are added to the export list
;;   to be registered in tc-toplevel
;; - imports are the set-difference of the union of all imports and the
;;   union of all exports
;; This seems to correctly handle both recursive and non-recursive
;; linking patterns
(define-syntax (define-values/invoke-unit/infer stx)
  (syntax-parse stx
    [dviui:define/invoke/infer-form
     #`(begin
         #,(internal (quasisyntax/loc stx
                       (define-values/invoke-unit-internal
                         (#,@(map imports/members (attribute dviui.inferred-imports)))
                         (#,@(process-dv-exports
                              (attribute dviui.inferred-exports))))))
         #,(ignore (quasisyntax/loc stx dviui.untyped-stx)))]))


;; Macros for the typed versions of invoke-unit and invoke-unit/infer
(define-syntax (invoke-unit/infer stx)
  (syntax-parse stx 
    [(_ . rest)
     (ignore
      (tr:unit:invoke-property
       (quasisyntax/loc stx (untyped-invoke-unit/infer . rest)) 'infer))]))

(define-syntax (invoke-unit stx)
  (syntax-parse stx
    [(invoke-unit . rest)
     (ignore
      (tr:unit:invoke-property
       (quasisyntax/loc stx
         (untyped-invoke-unit . rest)) #t))]))


(define-syntax (add-tags stx)
  (syntax-parse stx
    [(_) #'(begin)]
    [(_ e)
     (define exp-e (local-expand #'e (syntax-local-context) (kernel-form-identifier-list)))
     (syntax-parse exp-e
       #:literals (begin define-values define-syntaxes :)
       [(begin b ...)
        #'(add-tags b ...)]
       [(define-syntaxes (name:id ...) rhs:expr)
        exp-e]
       [(define-values () (colon-helper (: name:id type) rest ...))
        #`(define-values ()
            #,(tr:unit:body-exp-def-type-property
               #`(#%expression
                  (begin (void (lambda () #,(syntax-local-introduce #'name)))
                         (colon-helper (: name type) rest ...)))
               'def/type))]
       [(define-values (name:id ...) rhs)
        #`(define-values (name ...)
            #,(tr:unit:body-exp-def-type-property
               #'(#%expression
                  (begin
                    (void (lambda () name) ...)
                    rhs))
               'def/type))]
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

;; define-unit macro
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


;; Syntax classes and macro for typed compound-unit
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

  (define (build-compound-unit-prop import-tags all-import-links all-export-links)
    (define table
      (make-immutable-free-id-table
       (append
        (map cons
             import-tags
             (map (compose gensym syntax-e) import-tags))
        (map cons
             (flatten all-export-links)
             (map (compose gensym syntax-e) (flatten all-export-links))))))
    (define imports-tags
      (map (λ (id) (free-id-table-ref table id #f)) import-tags))
    (define units-exports
      (map
       (λ (lst) (map (λ (id) (free-id-table-ref table id #f)) lst))
       all-export-links))
    (define units-imports
      (map
       (λ (lst) (map (λ (id) (free-id-table-ref table id #f)) lst))
       all-import-links))
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


;; compound-unit/infer
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

;; unit-from-context forms
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
