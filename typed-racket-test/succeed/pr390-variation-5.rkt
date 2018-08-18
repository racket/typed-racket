#lang racket/base
(require rackunit)

;; Test contract generation
;; - HashTable should make 1 contract, not an or/c
;; - (U #f HashTable) should make an or/c with 2 things
;; (This file only generates contracts, it doesn't check that they are not redundant.)

(module u racket/base
  (define u-ihash (make-immutable-hash '((a . 1) (b . 2))))
  (define u-mhash (make-hash '((a . 1) (b . 2))))
  (provide u-ihash u-mhash))

(module t typed/racket
  (require/typed (submod ".." u)
    (u-mhash (Mutable-HashTable String String))
    (u-ihash (U Integer #f (Immutable-HashTable Symbol Integer) (Mutable-HashTable String String))))

  (define t-ihash : (HashTable Symbol Integer)
    (make-immutable-hash '((a . 1) (b . 2))))
  (define t-mhash : (HashTable Symbol Integer)
    (make-hash '((a . 1) (b . 2))))
  (provide u-ihash u-mhash t-ihash t-mhash))
(require 't racket/contract)

(check-false (value-contract u-ihash))
(check-false (value-contract t-ihash))
(check-pred value-contract u-mhash)
(check-pred value-contract t-mhash)
