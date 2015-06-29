#lang typed/racket
(: foo (-> Integer AnyValues))
(define (foo x)
  x)
(foo 5)
