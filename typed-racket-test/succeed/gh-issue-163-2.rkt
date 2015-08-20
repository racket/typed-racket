#lang racket

(module m typed/racket/base
  (define x 1)
  (provide x))

(module n typed/racket/base
  (require (submod ".." m))
  (module* a #f x))
