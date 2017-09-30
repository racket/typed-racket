#lang typed/racket

(require typed/rackunit)

(: f : (case-> (-> (Boxof String) Any)
               (-> (Boxof Integer) Any)))
(define (f x)
  (cast x Any))

(check-equal? (f (box "a")) (box "a"))
(check-equal? (f (box 1)) (box 1))

