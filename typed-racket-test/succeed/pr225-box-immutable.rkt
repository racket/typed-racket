#lang typed/racket

;; box-immutable should not break programs that use Boxof

(define b (box-immutable 5))

(: f : (Boxof Integer) -> Integer)
(define (f b)
  (unbox b))

(f b)

