#lang typed/racket #:with-refinements


(define n-1    : (Refine [x : Integer] (= x -1)) -1)
(define n0     : (Refine [x : Integer] (= x 0))   0)
(define n0*    : (Refine [x : Zero] (= x 0))      0)
(define n1     : (Refine [x : Integer] (= x 1))   1)
(define n2     : (Refine [x : Integer] (= x 2))   2)
(define n2*    : (Refine [x : Byte] (= x 2))      2)
(define n3     : (Refine [x : Integer] (= x 3))   3)
(define n42    : (Refine [x : Integer] (= x 42)) 42)
(define n42*   : (Refine [x : Byte] (= x 42))    42)
(define n42**  : (Refine [x : Fixnum] (= x 42))  42)

(define x 1)

(ann x One)
(ann x (Refine [x : One] (= x 1)))

(define y : Integer 1)

(define z : (Refine [v : Integer] (= v (* 2 y)))
  (* 2 y))

