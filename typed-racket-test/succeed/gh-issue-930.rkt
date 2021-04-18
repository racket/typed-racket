#lang racket/base

(module mod1 typed/racket/base
  (provide (all-defined-out))
  (struct a ([aa : Number]) #:type-name AAAAA)
  (struct ba a ())
  (struct bA AAAAA ()))

(module mod2 racket/base
  (require (submod ".." mod1))
  (define a2 (a 1))
  (a-aa (struct-copy a a2 [aa 2]))
  (a-aa (struct-copy AAAAA a2 [aa 2])))

