#lang typed/racket/shallow

;; Test `list-ref` applications

(: f (-> (Listof Natural) Natural))
(define (f x)
  (+ (car x)
     (first x)))

(f (list 4 4 4 4))

(: g (-> (Listof (Listof Natural)) Natural))
(define (g x)
  (define l (list-ref x (+ 0 0)))
  (+ (car l) (third l)))

(g '((1 2 2 3 4)))

(: h (-> (Pairof Natural Natural) Natural))
(define (h x)
  (+ (car x) (cdr x)))

(h '(1 . 2))
