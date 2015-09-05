#;
(exn-pred #rx"u: contract violation\n  expected: Integer")
#lang racket

(module typed typed/racket
  (provide (all-defined-out))
  (define-signature x^ ([x : Integer]))
  (define-signature y^ ([y : (-> Integer Integer)]))

  (define u
    (unit (import x^) (export y^) (define (y n) x))))

(module untyped racket
  (require (submod ".." typed))
  (define x 11)
  (define-values/invoke-unit u (import x^) (export y^))
  (y "not an integer"))

(require 'untyped)
