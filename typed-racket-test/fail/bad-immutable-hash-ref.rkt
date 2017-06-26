#lang typed/scheme

(: table (Immutable-HashTable Integer (-> Integer)))
(define table
  (make-immutable-hash null))

(: lookup (Integer -> Integer))
(define (lookup n)

  (: thunk (-> Integer))
  (define thunk
    (hash-ref table n (lambda () n)))

  (thunk))

(lookup 1)
