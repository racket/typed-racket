#lang typed/racket

(define-type (Promiseof A) (Boxof (U (→ (Listof A)) (Listof A))))
(struct: (A) Queue ([fld : (Promiseof A)]))

(define-syntax-rule (empty A)
  ((inst Queue A) (box (lambda: () '()))))

(: empty? : (All (A) ((Queue A) -> Boolean)))
(define (empty? q)
  (null? (Queue-fld q)))

;; make sure that inference works on nested 'Nothing' types
(empty? (empty Nothing))


