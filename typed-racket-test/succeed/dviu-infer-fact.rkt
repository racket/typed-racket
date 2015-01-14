#lang typed/racket

(define-signature fact^ ([fact : (-> Natural Natural)]))


(define-unit fact@
  (import (prefix i: fact^))
  (export fact^)
  (: fact (-> Natural Natural))
  (define (fact n)
    (if (= 0 n)
        1
        (* n (i:fact (sub1 n))))))

;; without link this should fail
(define-values/invoke-unit/infer (link fact@))

(fact 5)
