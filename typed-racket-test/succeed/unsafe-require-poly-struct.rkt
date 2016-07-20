#lang typed/racket/base

;; Test unsafe require with a polymorphic struct

(require typed/racket/unsafe)

(module a racket/base
  (struct foo (x y))
  (define a-foo (foo 1 2))
  (provide (struct-out foo) a-foo))

(unsafe-require/typed (submod "." a)
                      [#:struct (X Y) foo ([x : X] [y : Y])]
                      [a-foo (foo Integer Integer)])

(add1 (foo-x (foo 3 4)))
(add1 (foo-y a-foo))

(module b racket/base
  (struct bar (x y))
  (struct baz bar (z))
  (provide (struct-out bar) (struct-out baz)))

(unsafe-require/typed (submod "." b)
                      [#:struct (X Y) bar ([x : X] [y : Y])]
                      [#:struct (X Y Z) (baz bar) ([z : Z])])

(add1 (bar-x (baz 1 2 3)))
