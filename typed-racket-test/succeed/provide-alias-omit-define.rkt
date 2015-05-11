#lang racket
(module m typed/racket
  (provide x)
  (define-type x 'x #:omit-define-syntaxes)
  (define x : x 'x))
(module n typed/racket
  (require (submod ".." m))
  x ; works fine, outputs 'x
  (define y : x x))