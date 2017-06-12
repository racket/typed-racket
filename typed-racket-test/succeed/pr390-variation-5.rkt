#lang racket/base

;; Test contract generation
;; - HashTable should make 1 contract, not an or/c
;; - (U #f HashTable) should make an or/c with 2 things
;; (This file only generates contracts, it doesn't check that they are not redundant.)

(module u racket/base
  (define u-hash (make-immutable-hash '((a . 1) (b . 2))))
  (provide u-hash))

(module t typed/racket
  (require/typed (submod ".." u)
    (u-hash (U Integer #f (Immutable-HashTable Symbol Integer) (Mutable-HashTable String String))))

  (define t-hash : (HashTable Symbol Integer)
    (make-immutable-hash '((a . 1) (b . 2))))
  (provide u-hash t-hash))
(require 't racket/contract)

(void
  (hash-ref u-hash 'a)
  (hash-set u-hash 'c 3)

  (hash-ref t-hash 'a)
  (hash-set t-hash 'c 3))
