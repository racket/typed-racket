#;
(exn-pred #rx"mutation only allowed")
#lang typed/racket

;; Test type variable scope

;; The 'a' is bound in two different scopes
(plambda: (a) ([x : a])
  (plambda: (a) ([y : a]) (set! x y)))
