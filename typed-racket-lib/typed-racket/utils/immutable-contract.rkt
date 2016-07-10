#lang racket/base

(require racket/contract)
(provide immutable-hash/c)

(define (immutable-hash/c k v) (hash/c k v #:immutable #t))
