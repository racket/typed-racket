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
  (and/c hash? (not/c hash-weak?) (hash/c/check-key k v #:immutable #f)))

(define (immutable-hash/c k v)
  (and/c hash? (hash/c/check-key k v #:immutable #t)))

(define (weak-hash/c k v)
  (and/c hash? hash-weak? (hash/c/check-key k v #:immutable #f)))

(define (hash/c/check-key k v #:immutable [immutable 'dont-care])
  ;; TODO if (flat-contract? k), then make a contract that produces a "good"
  ;;      error message given a hashtable that is not a `hash-equal?`
  (hash/c k v #:immutable immutable))
