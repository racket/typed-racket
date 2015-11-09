#lang typed/racket

(: equal-string-length (→ String String Boolean))
(define (equal-string-length s1 s2)
  (= (string-length s1) (string-length s2)))

(member 123
        '("bb" "c" "ddd")
        equal-string-length)
