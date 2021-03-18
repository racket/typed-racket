#lang typed/racket/base

(: a-func (All ([X <: Integer])
               (-> X String)))
(define (a-func a)
  (number->string a))


(a-func 19.999) ;; fail
