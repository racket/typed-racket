#lang typed/racket/shallow

;; Expand, and make sure no shape checks,
;;  because cdr applied to a list is OK

(require racket/list racket/unsafe/ops)

(define x0 : (Listof Symbol) '(A B C))
(define x1 : (List Symbol Symbol) '(A B))
(define x2 : (Pairof Symbol (Listof Symbol)) (cons 'D x0))

(cdr x0)
(rest x0)
(cdr x1)
(cdr x2)
(unsafe-cdr x0)
(unsafe-cdr x1)
(unsafe-cdr x2)
