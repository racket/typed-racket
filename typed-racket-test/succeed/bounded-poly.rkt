#lang typed/racket/base

(: a-func (All ([X <: Integer])
               (-> X String)))
(define (a-func a)
  (number->string a))


(a-func 10) ;; pass


(struct (A) foo ([a : A]) #:type-name Foo)

(: c-func (All ([X <: Integer] [Y <: (Foo X)])
               (-> X Y String)))
(define (c-func a b)
  (number->string a))

(c-func 10 (foo 10))

(: d-func (All ([X <: Integer] [Y <: (Foo X)])
               (-> X Y String)))
(define (d-func a b)
  (number->string (foo-a b)))

(d-func 42 (foo 10))
