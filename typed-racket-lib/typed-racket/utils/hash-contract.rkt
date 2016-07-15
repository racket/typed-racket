#lang racket/base

(require racket/contract)
(provide mutable-hash/c immutable-hash/c weak-hash/c)

(define (mutable-hash/c k v) (and/c hash? (not/c hash-weak?) (hash/c k v #:immutable #f)))
(define (immutable-hash/c k v) (hash/c k v #:immutable #t))
(define (weak-hash/c k v) (and/c hash? hash-weak? (hash/c k v #:immutable #f)))
