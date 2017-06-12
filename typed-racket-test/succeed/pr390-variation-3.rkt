#lang typed/racket/base

;; Test filters for hash types

(: filter-HT (-> (HashTable Integer Integer) HashTableTop))
(define (filter-HT h)
  (cond
   [(immutable? h)
    (ann h (Immutable-HashTable Integer Integer))]
   [(hash-weak? h)
    (ann h (Weak-HashTable Integer Integer))]
   [else
    (ann h (Mutable-HashTable Integer Integer))]))

(: filter-U (-> (U #f (HashTable Integer Integer)) Any))
(define (filter-U maybe-ht)
  (cond
   [(immutable? maybe-ht)
    (ann maybe-ht (Immutable-HashTable Integer Integer))]
   [(and maybe-ht (hash-weak? maybe-ht))
    (ann maybe-ht Weak-HashTableTop)]
   [(hash? maybe-ht)
    (ann maybe-ht Mutable-HashTableTop)]
   [else
    (ann maybe-ht #f)]))

(void
  (filter-HT (make-immutable-hash '((1 . 1))))
  (filter-HT (make-hash '((1 . 1))))
  (filter-HT (make-weak-hash '((1 . 1))))

  (filter-U #f)
  (filter-U (make-immutable-hash '((1 . 1))))
  (filter-U (make-hash '((1 . 1))))
  (filter-U (make-weak-hash '((1 . 1)))))
