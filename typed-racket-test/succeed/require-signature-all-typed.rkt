#lang typed/racket

(module a typed/racket
  (provide foo^)

  (define-signature foo^
    ([n : Number])))

(require 'a)

(define-unit foo@
  (import)
  (export foo^)
  (define n 5))