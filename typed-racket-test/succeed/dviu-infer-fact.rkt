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
;; see the corresponding test for failure:
;; dviu-infer-fact-no-link.rkt in the fail directory
(define-values/invoke-unit/infer (link fact@))

(fact 5)
