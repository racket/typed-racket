#;
(exn-pre #rx"expected: Number.*given: \\(Listof Number\\)")
#lang typed/racket

(define (f [a : Integer 1] b : Integer *)
  (cons a b))

(add1 (cdr (f 1 2)))
