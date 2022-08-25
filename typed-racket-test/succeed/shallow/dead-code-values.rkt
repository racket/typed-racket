#lang typed/racket/shallow

;; Test the 'values' case in defender/defender protect-codomain,
;; ... application returns +1 results that need cod checks
;; ... but also send the rewritten code through the optimizer

(: forth-eval (-> Any Any (Values Symbol Symbol)))
(define (forth-eval E S)
  (match 'any
    [(? pair? E+S)
     ;; dead code
     (values (car E+S) (cdr E+S))]))
