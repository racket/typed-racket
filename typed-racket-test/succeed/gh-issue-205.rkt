#lang typed/racket

;; A test for GH issue #205

(struct (A) foo ([x : A]))

(struct (A) baz foo ())

(define (f [i : Integer]) : (foo Integer)
    (baz i))

(require typed/rackunit)

(check-equal? (if (baz? (f 1)) 1 2) 1)
