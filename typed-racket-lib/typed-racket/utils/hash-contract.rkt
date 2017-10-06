#lang racket/base

(require racket/contract)
(provide tr-hash/c mutable-hash/c immutable-hash/c weak-hash/c)

(define (mutable-hash/c k v)
  (and/c hash? (not/c hash-weak?) (tr-hash/c k v #:immutable #f)))

(define (immutable-hash/c k v)
  (tr-hash/c k v #:immutable #t))

(define (weak-hash/c k v)
  (and/c hash? hash-weak? (tr-hash/c k v #:immutable #f)))

(define (tr-hash/c k v #:immutable [immutable 'dont-care])
  (if (flat-contract? k)
    (hash/c k v #:immutable immutable)
    (and/c hash? hash-equal? (hash/c k v #:immutable immutable))))
