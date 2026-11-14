#;
(exn-pred #rx"could not be applied")
#lang typed/racket
(require racket/flonum)

;; Inferring map's result type eliminates id's type variables. Promoting
;; (Mutable-HashTable k v) to (Mutable-HashTable Any Any) gave an alias
;; through which anything could be stored into h, whose values typed
;; code then trusted to be flonums. A type that mentions an eliminated
;; variable in an invariant position must widen as a whole, here to
;; Mutable-HashTableTop, and so inference fails.

(: h (Mutable-HashTable Nothing Nothing))
(define h (make-hash))
(: id (All (k v) (-> (Mutable-HashTable k v) (Mutable-HashTable k v))))
(define (id x) x)
(hash-set! (car (map id (list h))) 1 "not a flonum")
(for ([(k v) (in-hash h)])
  (displayln (fl+ v 1.0)))
