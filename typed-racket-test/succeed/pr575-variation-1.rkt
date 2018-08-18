#lang typed/racket
(require typed/rackunit)

;; Test occurrence typing of `immutable?` on vectors

(: f (-> (Vectorof Integer) Integer))
(define (f v)
  (if (immutable? v)
    (begin
      (ann v (Immutable-Vectorof Real))
      0)
    (begin
      (ann v (Mutable-Vectorof Integer))
      1)))

(: g (-> (Vector Integer) Integer))
(define (g v)
  (if (immutable? v)
    (begin
      (ann v (Immutable-Vector Real))
      2)
    (begin
      (ann v (Mutable-Vector Integer))
      3)))

(define v0 (vector-immutable 1))
(define v1 (vector 1))

(check-equal? (list (f v0) (f v1) (g v0) (g v1))
              (range 4))
