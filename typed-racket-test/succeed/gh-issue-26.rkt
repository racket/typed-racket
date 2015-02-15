#lang typed/racket

(define-type T1 (Listof (U T2 Symbol)))
(define-type T2 (Setof (U T1 Symbol)))

(: x1 T1)
(define x1 (list (set 'foo)))

(: x2 T2)
(define x2 (set (list 'foo)))
