#lang typed/racket

(define: memo : (HashTable Natural String)  (make-immutable-hash empty))
(define strs '("Hello" "Goodbye"))

(for/fold: : (HashTable Natural String)
    ([memo : (HashTable Natural String)  (make-immutable-hash empty)])
    ([i : Natural  (in-naturals)] [str : String  (in-list strs)])
  (hash-set memo i str))

(let () ;;bg: same code should work with Immutable-Hash type
  (define: memo2 : (Immutable-HashTable Natural String)  (make-immutable-hash empty))
  (define strs2 '("Hello" "Goodbye"))

  (for/fold: : (Immutable-HashTable Natural String)
      ([memo2 : (Immutable-HashTable Natural String)  (make-immutable-hash empty)])
      ([i : Natural  (in-naturals)] [str : String  (in-list strs2)])
    (hash-set memo2 i str)))
