#lang racket/base

(provide
  trawl-for-property
  parse-internal-class-data
  make-internal-external-mapping
  internal-class-data)

(require
  syntax/parse
  racket/set
  (for-template racket/base typed-racket/private/class-literals))

(define-syntax-class name-pair
  (pattern (internal:id external:id)))

(define-syntax-class internal-class-data
  #:literal-sets (kernel-literals)
  #:literals (class-internal values)
  (pattern (let-values ([() (begin (quote-syntax
                                    (class-internal
                                     (#:forall type-parameter:id ...)
                                     (#:all-inits all-init-names:id ...)
                                     (#:init init-names:name-pair ...)
                                     (#:init-field init-field-names:name-pair ...)
                                     (#:init-rest (~optional init-rest-name:id))
                                     (#:optional-init optional-names:id ...)
                                     (#:field field-names:name-pair ...)
                                     (#:public public-names:name-pair ...)
                                     (#:override override-names:name-pair ...)
                                     (#:private privates:id ...)
                                     (#:private-field private-fields:id ...)
                                     (#:inherit inherit-names:name-pair ...)
                                     (#:inherit-field inherit-field-names:name-pair ...)
                                     (#:augment augment-names:name-pair ...)
                                     (#:pubment pubment-names:name-pair ...))
                                    #:local)
                                   (#%plain-app values))])
             _)
           #:with type-parameters #'(type-parameter ...)
           #:with all-init-internals #'(all-init-names ...)
           #:with init-internals #'(init-names.internal ...)
           #:with init-externals #'(init-names.external ...)
           #:with init-field-internals #'(init-field-names.internal ...)
           #:with init-field-externals #'(init-field-names.external ...)
           #:with optional-inits #'(optional-names ...)
           #:with field-internals #'(field-names.internal ...)
           #:with field-externals #'(field-names.external ...)
           #:with public-internals #'(public-names.internal ...)
           #:with public-externals #'(public-names.external ...)
           #:with override-internals #'(override-names.internal ...)
           #:with override-externals #'(override-names.external ...)
           #:with inherit-externals #'(inherit-names.external ...)
           #:with inherit-internals #'(inherit-names.internal ...)
           #:with inherit-field-externals #'(inherit-field-names.external ...)
           #:with inherit-field-internals #'(inherit-field-names.internal ...)
           #:with augment-externals #'(augment-names.external ...)
           #:with augment-internals #'(augment-names.internal ...)
           #:with pubment-externals #'(pubment-names.external ...)
           #:with pubment-internals #'(pubment-names.internal ...)
           #:with private-names #'(privates ...)
           #:with private-field-names #'(private-fields ...)))


;; Syntax (Syntax -> Any) -> Listof<Syntax>
;; Look through the expansion of the class macro in search for
;; syntax with some property (e.g., methods)
(define (trawl-for-property form accessor)
  (define (recur-on-all stx-list)
    (apply append (map (λ (stx) (trawl-for-property stx accessor))
                       (syntax->list stx-list))))
  (syntax-parse form
    #:literals (let-values letrec-values #%plain-app
                #%plain-lambda #%expression)
    [stx
     #:when (accessor #'stx)
     (list form)]
    [(let-values (b ...) body ...)
     (recur-on-all #'(b ... body ...))]
    ;; for letrecs, traverse the RHSs too
    [(letrec-values ([(x ...) rhs ...] ...) body ...)
     (recur-on-all #'(rhs ... ... body ...))]
    [(#%plain-app e ...)
     (recur-on-all #'(e ...))]
    [(#%plain-lambda (x ...) e ...)
     (recur-on-all #'(e ...))]
    [(#%expression e)
     (recur-on-all #'(e))]
    [_ '()]))

(define (parse-internal-class-data class-name-table)
  (syntax-parse class-name-table
    [tbl:internal-class-data
     ;; Save parse attributes to pass through to helper functions
     (define type-parameters (syntax->datum #'tbl.type-parameters))
     (define fresh-parameters (map gensym type-parameters))
     (hash 'type-parameters     type-parameters
           'fresh-parameters    fresh-parameters
           'optional-inits      (syntax->datum #'tbl.optional-inits)
           'only-init-internals (syntax->datum #'tbl.init-internals)
           'only-init-names     (syntax->datum #'tbl.init-externals)
           ;; the order of these names reflect the order in the class,
           ;; so use this list when retaining the order is important
           'init-internals      (syntax->datum #'tbl.all-init-internals)
           'init-rest-name     (and (attribute tbl.init-rest-name)
                                    (syntax-e (attribute tbl.init-rest-name)))
           'public-internals   (syntax->datum #'tbl.public-internals)
           'override-internals (syntax->datum #'tbl.override-internals)
           'pubment-internals  (syntax->datum #'tbl.pubment-internals)
           'augment-internals  (syntax->datum #'tbl.augment-internals)
           'method-internals
           (set-union (syntax->datum #'tbl.public-internals)
                      (syntax->datum #'tbl.override-internals))
           'field-internals
           (set-union (syntax->datum #'tbl.field-internals)
                      (syntax->datum #'tbl.init-field-internals))
           'inherit-internals
           (syntax->datum #'tbl.inherit-internals)
           'inherit-field-internals
           (syntax->datum #'tbl.inherit-field-internals)
           'init-names
           (set-union (syntax->datum #'tbl.init-externals)
                      (syntax->datum #'tbl.init-field-externals))
           'field-names
           (set-union (syntax->datum #'tbl.field-externals)
                      (syntax->datum #'tbl.init-field-externals))
           'public-names   (syntax->datum #'tbl.public-externals)
           'override-names (syntax->datum #'tbl.override-externals)
           'pubment-names  (syntax->datum #'tbl.pubment-externals)
           'augment-names  (syntax->datum #'tbl.augment-externals)
           'inherit-names  (syntax->datum #'tbl.inherit-externals)
           'inherit-field-names
           (syntax->datum #'tbl.inherit-field-externals)
           'private-names  (syntax->datum #'tbl.private-names)
           'private-fields (syntax->datum #'tbl.private-field-names)
           'overridable-names
           (set-union (syntax->datum #'tbl.public-externals)
                      (syntax->datum #'tbl.override-externals))
           'augmentable-names
           (set-union (syntax->datum #'tbl.pubment-externals)
                      (syntax->datum #'tbl.augment-externals))
           'method-names
           (set-union (syntax->datum #'tbl.public-externals)
                      (syntax->datum #'tbl.override-externals)
                      (syntax->datum #'tbl.augment-externals)
                      (syntax->datum #'tbl.pubment-externals))
           'all-internal
           (append (syntax->datum #'tbl.init-internals)
                   (syntax->datum #'tbl.init-field-internals)
                   (syntax->datum #'tbl.field-internals)
                   (syntax->datum #'tbl.public-internals)
                   (syntax->datum #'tbl.override-internals)
                   (syntax->datum #'tbl.inherit-internals)
                   (syntax->datum #'tbl.inherit-field-internals)
                   (syntax->datum #'tbl.pubment-internals)
                   (syntax->datum #'tbl.augment-internals))
           'all-external
           (append (syntax->datum #'tbl.init-externals)
                   (syntax->datum #'tbl.init-field-externals)
                   (syntax->datum #'tbl.field-externals)
                   (syntax->datum #'tbl.public-externals)
                   (syntax->datum #'tbl.override-externals)
                   (syntax->datum #'tbl.inherit-externals)
                   (syntax->datum #'tbl.inherit-field-externals)
                   (syntax->datum #'tbl.pubment-externals)
                   (syntax->datum #'tbl.augment-externals)))]))

(define (make-internal-external-mapping parse-info)
  (for/hash ([internal (hash-ref parse-info 'all-internal)]
             [external (hash-ref parse-info 'all-external)])
    (values internal external)))


