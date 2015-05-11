#lang typed/racket

(define-type T1 (Listof (U T2 Symbol)))
(define-type T2 (Setof (U T1 Symbol)))

(: x1 T1)
(define x1 (list (set 'foo)))

(: x2 T2)
(define x2 (set (list 'foo)))

;; Demonstrates a bug in the initial fix
(define-type S2 (U Null (Pairof String S2)))
(define-type S3 (U Null (Pairof Integer S3)))
(define-type S1 (Listof (U S1 S2 S3)))

(: y1 S1)
(define y1 (list (cons "foo" null)))

(: y2 S1)
(define y2 (list (cons 3 null)))
