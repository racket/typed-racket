#lang racket

(module typed typed/racket
  (provide (all-defined-out))
  (define-signature x^ ([x : Integer]))
  (define u (unit (import x^) (export) (lambda ([n : Integer]) (+ n x)))))

(module untyped racket
  (require (submod ".." typed))
  (define v (unit (import) (export x^) (define x 11)))
  (define w (compound-unit
             (import)
             (export)
             (link (([X : x^]) v)
                   (() u X))))

  (define f (invoke-unit w))
  (f 6))

(require 'untyped)
