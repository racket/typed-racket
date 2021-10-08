#;
(exn-pred #rx"Expected 1 type variables")
#lang typed/racket

;; Testing type variable scope

;; This should fail because of the type variable arities
(: f (All (b) (b -> b)))
(define f
  (plambda: (a b) ([x : b]) x))
