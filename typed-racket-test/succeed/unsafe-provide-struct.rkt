#lang racket/base

;; Test unsafe provide with a struct

(module a racket/base
  (struct foo (x y))
  (define a-foo (foo 1 2))
  (provide (struct-out foo) a-foo))

(module b typed/racket
  (require typed/racket/unsafe)
  (unsafe-provide (struct-out foo))
  (unsafe-require/typed (submod ".." a)
                        [#:struct foo ([x : String] [y : String])]
                        [a-foo foo]))
