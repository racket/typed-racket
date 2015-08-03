#lang racket

(module typed typed/racket
  (provide (all-defined-out))
  (define u 
    (unit (import)
          (export)
          (values (lambda ([x : String]) x) 
                  (lambda ([n : Integer]) (sqr n))))))

(module untyped racket
  (require (submod ".." typed))
  (define-values (f g) (invoke-unit u))
  (f "string")
  (g 5))

(require 'untyped)
