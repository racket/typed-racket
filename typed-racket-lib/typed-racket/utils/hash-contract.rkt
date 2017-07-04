#lang racket/base

;; Typed Racket custom hashtable contracts.
;; Goals:
;; - implement contracts for mutable/immutable/weak hashtables
;; - give a better error message than `racket/contract` when user
;;   tries to apply "a contract with a non-flat key"
;;   to a "hashtable that doesn't compare keys with equal?"

(require racket/contract)
(provide typed-racket-hash/c mutable-hash/c immutable-hash/c weak-hash/c)

(define (typed-racket-hash/c k v)
  (and/c hash? (hash/c/check-key k v)))

(define (mutable-hash/c k v)
  (and/c hash? hash-mutable? (hash/c/check-key k v #:immutable #f)))

(define (hash-mutable? h)
  (not (or (immutable? h) (hash-weak? h))))

(define (immutable-hash/c k v)
  (and/c hash? immutable? (hash/c/check-key k v #:immutable #t)))

(define (weak-hash/c k v)
  (and/c hash? hash-weak? (hash/c/check-key k v #:immutable #f)))

(define (hash/c/check-key k v #:immutable [immutable 'dont-care])
  (if (flat-contract? k)
    (hash/c k v #:immutable immutable)
    (and/c (flat-named-contract
            "hash-equal? (because the key contract is not a flat contract)"
            hash-equal?)
           (hash/c k v #:immutable immutable))))
