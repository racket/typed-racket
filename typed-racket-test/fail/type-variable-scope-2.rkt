#;
(exn-pred #rx"mutation only allowed")
#lang typed/racket

;; Testing type variable scope

;; This should fail for the same reason as PR 13123
(: i (All (x) (x -> x)))
(define i
  (plambda: (b) ([x : b])
    (plambda: (b) ([y : b])
      (set! x y))
    x))
