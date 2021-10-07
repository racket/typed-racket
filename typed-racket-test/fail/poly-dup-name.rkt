#;
(exn-pred #rx"duplicate type variable")
#lang typed/racket

;; don't allow duplicate type variable names

(: f (All (A A) (A -> (List A))))
(define (f a) (list a))
