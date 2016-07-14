#lang typed/racket/base

;; A require/typed of a polymorphic struct with a
;; parent is unsupported

(module a racket/base
  (struct foo (x y))
  (struct bar foo (z))
  (provide (struct-out foo)))

(require/typed (submod "." a)
               [#:struct (X Y) foo ([x : X] [y : Y])]
               [#:struct (Z) (bar foo) ([z : Z])])
