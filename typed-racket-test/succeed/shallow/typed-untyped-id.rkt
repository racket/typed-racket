#lang racket/base

(module a racket/base
  (require racket/contract)
  (provide
    (contract-out [f0 (-> symbol? symbol?)]))
  (define (f0 x) 'a))

(module b typed/racket/base
  (provide f1)
  (define (f1 (x : Symbol)) : Symbol 'a))

(module c racket/base
  (require (submod ".." a) (submod ".." b) typed/untyped-utils)
  (define-typed/untyped-identifier f f1 f0)
  (provide f))

(module d typed/racket/base/shallow
  ;; fail = (require (submod ".." c))
  (require/typed (submod ".." c)
                 (f (-> Symbol Symbol)))
  f)

(require 'd)
