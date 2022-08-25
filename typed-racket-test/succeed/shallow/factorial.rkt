#lang typed/racket/shallow

;; Test factorial function, and the `for/product` combinator

(: factorial (-> Natural Natural))
(define (factorial x)
  (for/product : Natural ([i : Natural (in-range x)])
    (+ i 1)))

(factorial 6)
