#lang typed/racket/base

(module+ test
  (require typed/rackunit)

  (define xs '(1 2))
  (define ys #(1 2))
  (define zs (hasheq 1 2 3 4))

  (check-eq? xs xs)
  (check-eq? ys ys)
  (check-eq? zs zs))
