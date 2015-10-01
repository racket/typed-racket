#lang racket/base

;; Performance test for generated contracts. This exists to measure
;; whether contract generation interferes with contract
;; system optimizations.

(module server typed/racket/base
  (: f (Integer -> Integer))
  (provide f)
  (define (f x) x))

(require (submod "." server))

(time
 (for ([x (in-range 100000)])
   (f 1) (f 2) (f 3) (f 4) (f 5)
   (f 1) (f 2) (f 3) (f 4) (f 5)
   (f 1) (f 2) (f 3) (f 4) (f 5)
   (f 1) (f 2) (f 3) (f 4) (f 5)
   (f 1) (f 2) (f 3) (f 4) (f 5)))
