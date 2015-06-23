#lang typed/racket/base
(: f : (All (A) (case-> [-> Zero]
                        [A -> A])))
(define f
  (case-lambda
    [() 0]
    [(a) a]))
(define (f0)
  (f))