#;
(exn-pred #rx"expected: One")
#lang typed/racket

;; test even? filter

(: foo (Integer -> String))
(define (foo n)
  (if (even? n)
      (o n)
      "dummy"))

(: o (One -> String))
(define (o x) "dummy")

