#lang typed/racket/base

(: my-remainder1 (-> Integer Fixnum Fixnum))
(define (my-remainder1 x y)
  (define-values [q r]
    (quotient/remainder x y))
  r)

(: my-remainder2 (-> Integer Fixnum Fixnum))
(define (my-remainder2 x y)
  (remainder x y))
