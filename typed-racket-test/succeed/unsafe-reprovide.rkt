#lang racket/base

;; Test `unsafe-reprovide` form,
;;  compare to `unsafe-provide`

(module source typed/racket
  (: v (-> Natural Natural))
  (define (v x)
    (add1 x))

  (provide v))

;; -----------------------------------------------------------------------------

(module adaptor-1 typed/racket
  (require (submod ".." source)
           typed/racket/unsafe)
  (unsafe-provide v))

(module adaptor-2 typed/racket
  (require (submod ".." source)
           typed/racket/unsafe)
  (unsafe-reprovide v))

(module adaptor-3 typed/racket
  (require (prefix-in source: (submod ".." source))
           typed/racket/unsafe)
  (define v source:v)
  (unsafe-provide v))

;; -----------------------------------------------------------------------------

(require
  (prefix-in s: (submod "." source))
  (prefix-in a1: (submod "." adaptor-1))
  (prefix-in a2: (submod "." adaptor-2))
  (prefix-in a3: (submod "." adaptor-3))
  (only-in racket/contract has-contract?)
  rackunit)

(check-true (has-contract? s:v))
(check-true (has-contract? a1:v))

(check-false (has-contract? a2:v))
(check-false (has-contract? a3:v))
