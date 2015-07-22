#;
(exn-pred "delay/name")
#lang racket

;; delay/name is a macro, so we wrap it with a function so we can type it:
(module untyped racket
  (provide delay/name/thunk)
  (define (delay/name/thunk f) (delay/name (f))))

;; Now we require/typed our function
(module typed-delay/name/thunk typed/racket
  (require/typed (submod ".." untyped) [delay/name/thunk (∀ (T) (→ (→ T) (Promise T)))])
  (provide delay/name/thunk))

;; This module shows the bug:
(module bug typed/racket
  (require (submod ".." typed-delay/name/thunk))

  (define mutable : (U 'a 'b) 'a)

  (define ab-mut (delay/name/thunk (λ () (begin0 mutable
                                                 (set! mutable 'b))))) ;; BAD BAD BAD!

  (ann (if (eq? (force ab-mut) 'a) ;; Here, (eq? (force ab-mut) 'a)
           (force ab-mut) ;; But here, (eq? (force ab-mut) 'b)
           #f)
       (U 'a #f))) ;; This typechecks, and throws no warning, but prints 'b !!!

(require 'bug)
