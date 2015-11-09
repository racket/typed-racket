#lang typed/racket

(: equal-string-length (→ String String Boolean))
(define (equal-string-length s1 s2)
  (= (string-length s1) (string-length s2)))

(assoc 123
       '(("bb" . 1) ("c" . 2) ("ddd" . 3))
       equal-string-length)
