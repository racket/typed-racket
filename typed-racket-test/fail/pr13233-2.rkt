#;
(exn-pred #rx"expected: One")
#lang typed/racket

;; test odd? filter

(: foo (Integer -> String))
(define (foo n)
  (if (odd? n)
      "dummy"
      (o n)))

(: o (One -> String))
(define (o x) "dummy")

