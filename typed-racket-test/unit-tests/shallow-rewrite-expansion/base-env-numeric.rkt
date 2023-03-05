#lang typed/racket/shallow

(let ((x : Nonnegative-Integer 5))
  (= x 0)
  (* x 1)
  (+ x 1))

