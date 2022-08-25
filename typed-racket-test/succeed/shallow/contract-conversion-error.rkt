#lang racket/load

;; two cases of arity 1, ok for shallow

(module a typed/racket/shallow (define v values) (provide v))
(require 'a)
v
