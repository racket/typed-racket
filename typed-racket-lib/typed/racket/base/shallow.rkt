#lang racket/base

(require
  (only-in racket/require subtract-in)
  (for-syntax racket/base)
  typed-racket/base-env/base-env-uncontracted
  typed-racket/utils/shallow-contract ;; needed for eval, see test in succeed/shallow/scribble-example.rkt
  (subtract-in
    (except-in typed/racket/base
               #%module-begin #%top-interaction with-type default-continuation-prompt-tag)
    (submod typed-racket/base-env/prims-contract forms))
  (only-in (submod typed-racket/base-env/prims-contract forms)
           make-predicate
           define-predicate)
  (only-in (submod typed-racket/base-env/prims-contract forms-shallow)
           (require-typed-signature-shallow require-typed-signature)
           (require/opaque-type-shallow require/opaque-type)
           (require-typed-struct-legacy-shallow require-typed-struct-legacy)
           (require-typed-struct-shallow require-typed-struct)
           (require/typed-legacy-shallow require/typed-legacy)
           (require/typed-shallow require/typed)
           (require/typed/provide-shallow require/typed/provide)
           (require-typed-struct/provide-shallow require-typed-struct/provide)
           (cast-shallow cast))
  (only-in typed-racket/typed-racket with-type-shallow)
  (only-in typed/racket/base
           [#%module-begin --#%module-begin]
           [#%top-interaction --#%top-interaction]))

(provide
  (all-from-out typed/racket/base)
  (all-from-out typed-racket/base-env/base-env-uncontracted)
  (all-from-out (submod typed-racket/base-env/prims-contract forms-shallow))
  make-predicate define-predicate
  with-type
  (rename-out [-#%module-begin #%module-begin] [-#%top-interaction #%top-interaction]))

(define-syntax (-#%module-begin stx)
  (quasisyntax/loc stx (--#%module-begin #:shallow . #,(cdr (syntax-e stx)))))

(define-syntax (-#%top-interaction stx)
  (quasisyntax/loc stx (--#%top-interaction #:shallow . #,(cdr (syntax-e stx)))))

(define-syntax (with-type stx)
  (quasisyntax/loc stx (with-type-shallow . #,(cdr (syntax-e stx)))))

