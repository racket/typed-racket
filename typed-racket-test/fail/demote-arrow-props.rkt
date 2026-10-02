#;
(exn-pred #rx"could not be applied")
#lang typed/racket

;; Inferring map's argument type eliminates call's type variable from
;; (-> Any Boolean : a). Demotion dropped that function type's only case,
;; leaving a type that every function satisfies, so a function on
;; integers was accepted and then applied to a string. Demotion must
;; strengthen the propositions instead.

(: call (All (a) (-> (-> Any Boolean : a) Boolean)))
(define (call p) (p "hello"))
(map call (list (λ ([x : Integer]) (= (add1 x) 1))))
