#lang racket

(module example typed/racket/shallow
  (: id (Procedure -> Procedure))
  (define (id x) x)

  (: f (Integer -> Integer))
  (define (f x) (+ x 1))

  (define g (id f))

  (provide g))

(require 'example)

(g 3)
