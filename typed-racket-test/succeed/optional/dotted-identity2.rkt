#lang typed/racket/optional

(: f (All (a ...) ((a ... a -> Integer) -> (a ... a -> Integer))))
(define (f x) x)

(: g (All (b ...) ( -> (b ... b -> Integer))))
(define (g) (lambda xs 0))

(f (g))
