#lang racket/base

(module u racket/base
  (struct foo (a))
  (provide (struct-out foo)))

(module t typed/racket
  (require/typed/provide (submod ".." u)
    (#:struct foo ((a : Symbol))))
  (define ins : foo (foo 'hello))
  (if (foo? ins)
      (foo-a ins)
      (error 'test "not going to happen")))

(require 't racket/match)
(match (foo 'a)
  [(foo a) a])
