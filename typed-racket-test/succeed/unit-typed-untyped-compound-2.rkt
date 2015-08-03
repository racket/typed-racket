#lang racket

(module untyped racket
  (provide (all-defined-out))
  (define-signature x^ (x))
  (define u (unit (import x^) (export) (lambda (n) (+ n x)))))

(module typed typed/racket
  (require/typed (submod ".." untyped)
    [#:signature x^ ([x : Integer])]
    [u (Unit (import x^) (export) (-> Integer Integer))])
  (define v (unit (import) (export x^) (define x 11)))
  (define w (compound-unit
             (import)
             (export)
             (link (([X : x^]) v)
                   (() u X))))

  (define f (invoke-unit w))
  (f 6))

(require 'typed)
