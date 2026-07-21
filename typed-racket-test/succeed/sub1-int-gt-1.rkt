#lang typed/racket

;; From this stack overflow question by Lyu Yao Ting:
;; Type mismatch issue of sub1
;; https://stackoverflow.com/questions/58887937/type-mismatch-issue-of-sub1

(define-predicate one? One)
(: pick1 (-> Positive-Integer (Listof Any) Any))
(define pick1
  (λ (n lat)
    (cond [(one? n) (car lat)]
          [else (pick1 (sub1 n)
                       (cdr lat))])))
