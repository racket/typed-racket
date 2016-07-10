#lang typed/racket/base

(require typed/rackunit)

;; All tests should work with `HashTable Integer Symbol` as the return type.
(check-pred
 hash?
 (for/hash: : (HashTable Integer Symbol)
            ((a (list 1 2 3)))
  (values a 'a)))

(check-pred
 hash?
 (for/hash: : (Immutable-HashTable Integer Symbol)
            ((a (list 1 2 3)))
  (values a 'a)))

(check-pred
 hash?
 (for/hash: : (Immutable-HashTable Integer Symbol)
            ((a (list 1 2 3))
             (b '(a b c)))
  (values a b)))

(check-pred
 hash?
 (for*/hasheq: : (Immutable-HashTable Integer Symbol)
             ((a (list 1 2 3))
              (b '(a b c)))
  (values a b)))


(check-pred
 hash-eq?
 (for/hasheq: : (Immutable-HashTable Integer Symbol)
            ((a (list 1 2 3)))
  (values a 'a)))

(check-pred
 hash-eq?
 (for/hasheq: : (Immutable-HashTable Integer Symbol)
            ((a (list 1 2 3))
             (b '(a b c)))
  (values a b)))

(check-pred
 hash-eq?
 (for*/hasheq: : (Immutable-HashTable Integer Symbol)
             ((a (list 1 2 3))
              (b '(a b c)))
  (values a b)))


(check-pred
 hash-eqv?
 (for/hasheqv: : (Immutable-HashTable Integer Symbol)
            ((a (list 1 2 3)))
  (values a 'a)))

(check-pred
 hash-eqv?
 (for/hasheqv: : (Immutable-HashTable Integer Symbol)
            ((a (list 1 2 3))
             (b '(a b c)))
  (values a b)))

(check-pred
 hash-eqv?
 (for*/hasheqv: : (Immutable-HashTable Integer Symbol)
             ((a (list 1 2 3))
              (b '(a b c)))
  (values a b)))



(for*/hash: : (Immutable-HashTable Number Number)
            ((v : Number '(1 2 3))
             (x : Number '(4 5 6)))
     (values v x))



(for/hash: : (Immutable-HashTable Symbol Symbol)
    ((v : Symbol '(a b c)))
  (values v v))

(for/hash: : (Immutable-HashTable Symbol Symbol)
    ([(k b) (in-hash (make-immutable-hash '((a . a) (b . b))))])
  (values k b))
