#lang racket/base

(module a typed/racket
  (require/typed racket/base [values (-> String String)])
  (provide values))

(module b typed/racket
  (require/typed (submod ".." a) [values (-> String Any)])
  values)

(require 'b)
