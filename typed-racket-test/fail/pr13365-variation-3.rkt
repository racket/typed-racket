#;
(exn-pred #rx"mismatch in.*a ...")
#lang typed/racket

(: f (All (a ...) (a ... a -> (Values a ... a))))
(define (f . x) (values 1))
