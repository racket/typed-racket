#;
(exn-pred #rx"duplicate type variable or index")
#lang typed/racket

;; don't allow duplicate names in indexes and tvars

(: f (All (A A ...) (A A ... A -> (List A ... A))))

(define (f a . xs)
  (map (λ: ([x : A]) a) xs))
