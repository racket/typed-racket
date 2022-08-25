#;
(exn-pred exn:fail:contract? #rx"shape-check")

#lang typed/racket/base/shallow

(module u racket/base
  (provide f)
  (define (f x)
    (values x x)))

(require/typed 'u
  (f (-> Natural (Values (Boxof Symbol) Natural))))

(define-values [a b] (f 2))

(unbox a)
