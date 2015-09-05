#;
(exn-pred #rx"define-values/invoke-unit: unit argument expects an untagged import with signature fact\\^")
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
;; see the corresponging successful test
;; dviu-infer-fact.rkt in the succeed directory
(define-values/invoke-unit/infer fact@)

(fact 5)
