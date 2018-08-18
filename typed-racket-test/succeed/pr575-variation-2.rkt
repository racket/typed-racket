#lang typed/racket/base

;; This code won't compile if `for/vector` accidentally generalizes the
;;  type of the vector it is building (from Mutable-Vector to Vector)

(require racket/flonum)

(: flvector->vector (FlVector -> (Vectorof Float)))
(define (flvector->vector xs)
  (for/vector: #:length (flvector-length xs) ([x  (in-flvector xs)]) : Flonum
    x))

(void (flvector->vector (flvector 3.14)))
