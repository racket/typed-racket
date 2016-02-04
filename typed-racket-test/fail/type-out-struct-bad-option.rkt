#lang racket/base

;; #:omit-constructor is the only option

(module struct-def typed/racket/base
  (provide (type-out
    (struct A ((a : String)) #:type-name Foo)))
  (struct A ((a : String)) #:type-name Foo))
