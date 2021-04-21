#lang racket/base
(module a racket/base
  (struct foo ())
  (provide (struct-out foo)))

(module b typed/racket/base
  (require/typed (submod ".." a) [#:struct foo ()])
  (provide (struct-out foo)))

(module c racket/base
  (require (submod ".." b))
  (provide (struct-out foo)))
