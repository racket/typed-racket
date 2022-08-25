#;
(exn-pred exn:fail:contract? #rx"unbox")

#lang typed/racket/base/optional

(module u racket/base
  (provide f)
  (define (f x)
    (values x x)))

(require/typed 'u
  (f (-> Natural (Values (Boxof Symbol) Natural))))

(define-values [a b] (f 2))

(unbox a)
