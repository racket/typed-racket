#lang typed/racket/gui/no-check

(: f (Integer -> Any))
(define (f x) (add1 (current-eventspace)))

(lambda (#{x : String}) (string-append " " x))

