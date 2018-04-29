#lang racket/load

;; two cases of arity 1, fine for optional

(module a typed/racket/optional (define v values) (provide v))
(require 'a)
v
