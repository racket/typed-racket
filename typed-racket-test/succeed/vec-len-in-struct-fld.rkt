#lang typed/racket/base

(struct: Foo ([fld : VectorTop]) #:transparent)

(: bar (Foo -> Index))
(define (bar p)
  (define n (vector-length (Foo-fld p)))
  (if (zero? n) 0 (- n 1)))
