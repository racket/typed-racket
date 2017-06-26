#lang typed/racket/base

;; Test Hash Table functions with "complicated" types

(require typed/rackunit)

(test-case "hash-copy" ;; returns a mutable hash with same 'key-holding strength'
  (check-pred hash?
    (ann (hash-copy (make-immutable-hash)) Mutable-HashTableTop))
  (check-pred hash?
    (ann (hash-copy (make-immutable-hash '((1 . 1)))) (Mutable-HashTable Integer Integer)))

  (check-pred hash?
    (ann (hash-copy (make-hash '((1 . 1)))) (Mutable-HashTable Integer Integer)))
  (check-pred hash?
    (ann (hash-copy (make-hash)) Mutable-HashTableTop))

  (check-pred hash-weak?
    (ann (hash-copy (make-weak-hash '((1 . 1)))) (Weak-HashTable Integer Integer)))
  (check-pred hash-weak?
    (ann (hash-copy (make-weak-hash)) Weak-HashTableTop)))

(test-case "hash-copy-clear" ;; returns hash with same mutability
  (check-pred immutable?
    (ann (hash-copy-clear (make-immutable-hash)) (Immutable-HashTable Any Any)))
  (check-pred immutable?
    (ann (hash-copy-clear (make-immutable-hash '((a . b)))) (Immutable-HashTable Symbol Symbol)))

  (check-pred hash?
    (ann (hash-copy-clear (make-hash)) Mutable-HashTableTop))
  (check-pred hash?
    (ann (hash-copy-clear (make-hash '((a . b)))) (Mutable-HashTable Symbol Symbol)))

  (check-pred hash-weak?
    (ann (hash-copy-clear (make-weak-hash)) Weak-HashTableTop))
  (check-pred hash-weak?
    (ann (hash-copy-clear (make-weak-hash '((a . b)))) (Weak-HashTable Symbol Symbol))))

(test-case "hash-remove" ;; only for immutable hashtables, but the TR type allows mutable/weak
  (check-pred immutable?
    (ann (hash-remove (make-immutable-hash '((a . A))) 'a) (Immutable-HashTable Symbol Symbol)))

  (check-exn exn:fail:contract?
    (λ () (hash-remove (make-hash '((a . A))) 'a)))

  (check-exn exn:fail:contract?
    (λ () (hash-remove (make-weak-hash '((a . A))) 'a))))
