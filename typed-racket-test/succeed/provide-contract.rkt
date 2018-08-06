#lang typed/racket
(: f (-> Integer Integer))
(define (f x) (+ 1 x))

(define f-ctc any/c)

(provide/contract
 [f f-ctc])
