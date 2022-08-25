#lang typed/racket/base/shallow

;; Test that type variables do not have a run-time check
;;  (no need for a check, because a context cannot make assumptions about the value)

(: f (All (A) (-> (Vectorof A) A)))
(define (f x)
  (vector-ref x 0))

(f (vector 1 2 3))
(f '#(four five six))
