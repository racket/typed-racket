#lang typed/racket/shallow

(: factorial (-> Natural Natural))
(define (factorial n)
  (if (zero? n)
    1
    (* n (factorial (- n 1)))))

(factorial 6)
