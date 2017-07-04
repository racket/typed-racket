#lang racket/base
(require racket/contract rackunit)

;; Spot-check contracts generated for vector types

(module t typed/racket/base
  (define mv : (Mutable-Vectorof Integer) (vector 1 2 3))
  (define iv : (Immutable-Vectorof Integer) '#(1 2 3))
  (define vm : (Vectorof Integer) (vector 1 2 3))
  (define vi : (Vectorof Integer) '#(1 2 3))

  (define mhv : (Mutable-Vector Integer) (vector 1))
  (define ihv : (Immutable-Vector Integer) '#(1))
  (define hvm : (Vector Integer) (vector 1))
  (define hvi : (Vector Integer) '#(1))

  (provide mv iv vm vi mhv ihv hvm hvi))
(require 't)

(check-pred value-contract mv)
(check-false (value-contract iv))
(check-pred value-contract vm)
(check-false (value-contract vi))

(check-pred value-contract mhv)
(check-false (value-contract ihv))
(check-pred value-contract hvm)
(check-false (value-contract hvi))
