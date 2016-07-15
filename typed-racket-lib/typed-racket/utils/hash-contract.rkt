#lang racket/base

(require racket/contract)
(provide mutable-hash/c immutable-hash/c weak-hash/c)

(define (mutable-hash/c k v) (and/c (hash/c k v #:immutable #f) (not/c hash-weak?)))
(define (immutable-hash/c k v) (hash/c k v #:immutable #t))
(define (weak-hash/c k v) (and/c (hash/c k v #:immutable #f) hash-weak?))
