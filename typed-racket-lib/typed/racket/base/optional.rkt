#lang racket/base

(require
  (only-in racket/require subtract-in)
  (for-syntax racket/base)
  typed-racket/base-env/base-env-uncontracted
  (subtract-in
    (except-in typed/racket/base
               #%module-begin #%top-interaction with-type default-continuation-prompt-tag)
    (submod typed-racket/base-env/prims-contract forms))
  (only-in (submod typed-racket/base-env/prims-contract forms)
           make-predicate
           define-predicate)
  (only-in (submod typed-racket/base-env/prims-contract forms-optional)
           (require-typed-signature-optional require-typed-signature)
           (require/opaque-type-optional require/opaque-type)
           (require-typed-struct-legacy-optional require-typed-struct-legacy)
           (require-typed-struct-optional require-typed-struct)
           (require/typed-legacy-optional require/typed-legacy)
           (require/typed-optional require/typed)
           (require/typed/provide-optional require/typed/provide)
           (require-typed-struct/provide-optional require-typed-struct/provide)
           (cast-optional cast))
  (only-in typed-racket/typed-racket with-type-optional)
  (only-in typed/racket/base
           [#%module-begin --#%module-begin]
           [#%top-interaction --#%top-interaction]))

(provide
  (all-from-out typed/racket/base)
  (all-from-out typed-racket/base-env/base-env-uncontracted)
  (all-from-out (submod typed-racket/base-env/prims-contract forms-optional))
  make-predicate define-predicate
  with-type
  (rename-out [-#%module-begin #%module-begin] [-#%top-interaction #%top-interaction]))

(define-syntax (-#%module-begin stx)
  (quasisyntax/loc stx (--#%module-begin #:optional . #,(cdr (syntax-e stx)))))

(define-syntax (-#%top-interaction stx)
  (quasisyntax/loc stx (--#%top-interaction #:optional . #,(cdr (syntax-e stx)))))

(define-syntax (with-type stx)
  (quasisyntax/loc stx (with-type-optional . #,(cdr (syntax-e stx)))))

