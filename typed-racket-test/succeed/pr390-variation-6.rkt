#lang racket/base

(module t typed/racket
  (provide h)
  (define h : (U (Immutable-HashTable Symbol Any) (Mutable-HashTable Symbol Any))
    (hash 'a 1)))

(require 't)
(hash-ref h 'a)
