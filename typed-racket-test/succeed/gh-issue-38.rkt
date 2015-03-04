#lang racket

(module untyped racket
  (provide
   (contract-out
    [f (-> number? number? number?)]
    [g (case-> (-> number? number? number?))]))
  (define (f x y)
    (expt x y))
  (define (g x y)
    (expt x y)))

(module type-env typed-racket/base-env/extra-env-lang
  (require (submod ".." untyped))
  (type-environment
   [f (-> -Number -Number -Number)]
   [g (-> -Number -Number -Number)]))

(module typed typed/racket
  (require (submod ".." type-env)
           typed/rackunit)
  (check-equal? (f 2 4) (g 2 4)))

(require 'typed)
