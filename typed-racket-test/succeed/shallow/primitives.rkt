#lang typed/racket/shallow

;; Test trusted primitives

(rest '(1 2 3))
(apply + '(1 2 3))

(struct foo ())
(foo? 4)
