#lang typed/racket

(: f : (Listof Integer) (Listof Integer) → Integer)
(define (f xs ys)
  (match* (xs ys)
    [((list a b) (or (list a b) (list b a))) (+ a b)]
    [(_ _) 42]))