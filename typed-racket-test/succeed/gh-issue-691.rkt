#lang typed/racket/base

(module mod1 typed/racket/base
  (define-struct/exec adder^ ([name : Symbol])
    [(λ (this n) (add1 n)) : (-> adder^ Number Number)])

  (define v^
    (adder^ 'v))

  (apply v^ '(41)))

(struct adder ([name : Symbol])
  #:property prop:procedure
  (λ ([this : adder] [n : Number]) : Number
    (add1 n)))

(define v
  (adder 'v))

(apply v '(41))

(define (foo [v : (Intersection (-> String String) (-> Integer Integer))]) : Integer
  (apply v '(10)))
