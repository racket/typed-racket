#lang typed/racket/base/shallow

;; Test `call-with-values` because it has a special typing rule

(call-with-values (lambda () '42) (lambda ((n : Real)) (+ n 1)))
(call-with-values (lambda () (values 1 42)) (lambda ((n : Real) (m : Real)) (+ n m)))
