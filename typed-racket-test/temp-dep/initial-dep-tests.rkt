#lang typed/racket

(require dependentfuns)

(define v : (Vectorof Integer) (vector 42 42 42 42))


(define (foo [i : Integer])
  (if (and (int< i (exact-vector-length v))
           (int<= 0 i))
      (safe-vector-ref v i)
      "Hi"))


(define (bar [n : Integer]
             [m : Integer])
  (if (and (int<= 0 m)
           (int<= 0 n)
           (int< (int+ n m) (exact-vector-length v)))
      (values (safe-vector-ref v n)
              (safe-vector-ref v m))
      (values "Hi" "there")))


(define (baz [n : Integer]
             [m : Integer])
  (if (and (int<= 0 n)
           (int<= n (int*2 m))
           (int< (int*3 m) (exact-vector-length v)))
      (values (safe-vector-ref v n)
              (safe-vector-ref v (int*2 m)))
      (values "Hi" "there")))




;; produces:
;; 42
;; 42
;; 42
;; 42
;; 42