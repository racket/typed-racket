#lang racket/base

;; Example from Redex,
;;  test that exporting a (HashTable Symbol Any) across a type boundary
;;  does not generate an `or/c` where multiple clauses might match.

;; The original Redex module is:
;;   `redex-lib/redex/private/env.rkt`

(module t-hash typed/racket/base
  (provide h)
  (define h : (HashTable Symbol Any) (hash)))

(require 't-hash rackunit)
(check-pred hash? h)


;; Same example, using vectors

(module t-vectorof typed/racket/base
  (provide v)
  (define v : (Vectorof Any) (vector)))

(require 't-vectorof)
(check-pred vector? v)
