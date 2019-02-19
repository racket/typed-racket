#lang racket/base

;; This module defined both Typed Racket-internal forms (such as
;; `define-type-alias-internal`, which is part of the expansion of
;; `define-type`) as well as syntax classes that recognize them and
;; functions that create them.

;; Since the forms themselves are needed by all Typed Racket programs,
;; they are defined in a `forms` submodule that does _not_ depend on
;; this module itself. That module has a `literal-set` submodule that
;; defines a literal set for use with `syntax/parse`. The further
;; submodule is again to avoid dependency.

(require
  syntax/parse/pre
  (for-syntax racket/base racket/syntax syntax/parse/pre)
  (for-label racket/base) ;; used for identifier comparison only
  (for-template racket/base))

(provide
  internal

  type-alias
  new-subtype-def
  type-refinement
  typed-struct
  typed-struct/exec
  typed-require
  typed-require/struct
  predicate-assertion
  type-declaration
  typed-define-signature
  typed-define-values/invoke-unit
  assert-typecheck-failure
  typecheck-failure

  type-alias?
  new-subtype-def?
  typed-struct?
  typed-struct/exec?
  typed-define-signature?
  typed-define-values/invoke-unit?)

(module forms racket/base
  (require (for-syntax racket/base))
  ;; Forms
  (define-syntax-rule (internal-forms set-name nms ...)
    (begin
      (provide nms ...)
      (module* literal-set #f
        (require syntax/parse/pre)
        (provide set-name)
        (define-literal-set set-name (nms ...)))
      (define-syntax (nms stx)
        (raise-syntax-error 'typecheck "Internal typechecker form used out of context" stx)) ...))

  (internal-forms internal-literals
                  require/typed-internal
                  define-type-alias-internal
                  define-new-subtype-internal
                  define-type-internal
                  define-typed-struct-internal
                  define-typed-struct/exec-internal
                  assert-predicate-internal
                  declare-refinement-internal
                  :-internal
                  assert-typecheck-fail-internal
                  typecheck-fail-internal
                  define-signature-internal
                  define-values/invoke-unit-internal))

(require (submod "." forms) (submod "." forms literal-set))



;;; Helpers

(define-splicing-syntax-class dtsi-fields
  #:attributes (mutable prefab type-only maker extra-maker [prop 1] [prop-val 1])
 (pattern
  (~seq
    (~or (~optional (~and #:mutable (~bind (mutable #t))))
         (~optional (~and #:prefab (~bind (prefab #t))))
         (~optional (~and #:type-only (~bind (type-only #t))))
         (~optional (~seq #:extra-maker extra-maker))
         (~optional (~seq #:maker maker))
         (~seq #:property prop prop-val))
    ...)))

(define-syntax-class struct-name
 (pattern nm:id)
 (pattern (nm:id parent:id)))


(define-syntax-class define-typed-struct-body
  #:attributes (name type-name mutable prefab type-only maker extra-maker nm
                     (tvars 1) (fields 1) (types 1) properties)
  (pattern ((~optional (tvars:id ...) #:defaults (((tvars 1) null)))
            nm:struct-name type-name:id ([fields:id : types:expr] ...) options:dtsi-fields)
           #:attr name #'nm.nm
           #:attr mutable (attribute options.mutable)
           #:attr prefab (attribute options.prefab)
           #:attr type-only (attribute options.type-only)
           #:attr maker (or (attribute options.maker) #'nm.nm)
           #:attr extra-maker (attribute options.extra-maker)
           #:attr properties (for/list ([p (attribute options.prop)]
                                        [pv (attribute options.prop-val)])
                               (list p pv))))

(define-syntax-class dviu-import/export
  (pattern (sig-id:id member-id:id ...)
           #:with sig #'sig-id
           #:with members #'(member-id ...)))

;;; Internal form syntax matching

(define-literal-set internal-form-literals #:for-label
  (values))

(define-syntax-class internal^
   #:attributes (value)
   #:literal-sets (kernel-literals internal-form-literals)
   (pattern (define-values () (begin (quote-syntax value:expr #:local) (#%plain-app values))))
   ;; handles form that splicing-syntax-parameterize expands to
   (pattern (define-values () (let-values () (quote-syntax value:expr #:local) (#%plain-app values))))
   ;; for use in forms like classes that transform definitions
   (pattern (let-values ([() (begin (quote-syntax value:expr #:local) (#%plain-app values))])
              (#%plain-app void))))

(define-syntax (define-internal-classes stx)
  (define-syntax-class clause
    (pattern [name:id (lit:id . body:expr)]
             #:with pred (format-id #'name "~a?" #'name)))

  (syntax-parse stx
    [(_ :clause ...)
     (syntax
      (begin
        (begin
          (define-syntax-class name
            #:auto-nested-attributes
            #:literal-sets ((internal-literals #:at name))
            (pattern i:internal^ #:with (lit . body) #'i.value))
          (define pred
            (syntax-parser
              [(~var _ name) #t]
              [_ #f])))
        ...))]))


(define-internal-classes
  [type-alias
    (define-type-alias-internal name type args)]
  [new-subtype-def
    (define-new-subtype-internal name (constructor rep-type) #:gen-id gen-id)]
  [type-refinement
    (declare-refinement-internal predicate)]
  [typed-struct
    (define-typed-struct-internal . :define-typed-struct-body)]
  [typed-struct/exec
    (define-typed-struct/exec-internal nm type-name ([fields:id : types] ...) proc-type)]
  [typed-require
    (require/typed-internal name type)]
  [typed-require/struct
    (require/typed-internal name type #:struct-maker parent)]
  [predicate-assertion
    (assert-predicate-internal type predicate)]
  [type-declaration
    (:-internal id:identifier type)]
  ;; the check field indicates whether this signature is being
  ;; required from an untyped context in which case the super
  ;; value is ignored and information about parent signatures
  ;; is inferred from static information bound to the signature
  ;; identifier
  [typed-define-signature
   (define-signature-internal name #:parent-signature super (binding ...) #:check? check)]
  ;; This should be a decent initial attempt at making
  ;; define-values/invoke-unit work, the unit-expr is
  ;; unnecessary at this point since it will be handled
  ;; when expressions are typechecked
  [typed-define-values/invoke-unit
   (define-values/invoke-unit-internal (import:dviu-import/export ...)
                                       (export:dviu-import/export ...))])

;; Define separately outside of `define-internal-classes` since this form
;; is meant to appear in expression positions, so it doesn't make sense to use
;; the `define-values` protocol used for other internal forms.
(define-syntax-class typecheck-failure
  #:literal-sets (kernel-literals internal-literals)
  (pattern (quote-syntax (typecheck-fail-internal stx message:str var) #:local)))

(define-syntax-class assert-typecheck-failure
  #:literal-sets (kernel-literals internal-literals)
  (pattern (quote-syntax (assert-typecheck-fail-internal body:expr) #:local)))


;;; Internal form creation
(define (internal stx)
  (quasisyntax/loc stx
    (define-values ()
      (begin
        (quote-syntax #,stx #:local)
        (#%plain-app values)))))
