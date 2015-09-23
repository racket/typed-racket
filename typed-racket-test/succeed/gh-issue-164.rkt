#lang typed/racket/base

;; Test for GH issue 164

(: x (U (Pairof (U) Any)
        (Pairof 'a Number)))
(define x (cons 'a 1))

(if (eq? (car x) 'a)
    (+ (cdr x) 1)
    0)

(if (eq? 'a (car x))
    (+ (cdr x) 1)
    0)
