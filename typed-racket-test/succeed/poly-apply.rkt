#lang typed/racket

(: my-apply (All (a ...) ((Any ... a -> Any) a ... a -> Any)))
(define (my-apply f . x) (apply f x))

(: id (All (a) (a -> a)))
(define (id x) x)

(my-apply id 'y)
