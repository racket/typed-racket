#lang typed/racket

(let ()
  (define: memo : (HashTable Natural String)  (make-immutable-hash empty))
  (define strs '("Hello" "Goodbye"))

  (for/fold: : (HashTable Natural String)
      ([memo : (HashTable Natural String)  (make-immutable-hash empty)])
      ([i : Natural  (in-naturals)] [str : String  (in-list strs)])
    (hash-set memo i str)))


;;bg: same code should work with Immutable-Hash type
(let ()
  (define: memo : (Immutable-HashTable Natural String)  (make-immutable-hash empty))
  (define strs '("Hello" "Goodbye"))

  (for/fold: : (Immutable-HashTable Natural String)
      ([memo : (Immutable-HashTable Natural String)  (make-immutable-hash empty)])
      ([i : Natural  (in-naturals)] [str : String  (in-list strs)])
    (hash-set memo i str)))
