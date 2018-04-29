#lang typed/racket/base/shallow

(module uuu racket/base
  (provide bad-sym)
  (define bad-sym #f))

(module opt typed/racket/base/optional
  (require/typed/provide (submod ".." uuu)
    (bad-sym Symbol)))

(require 'opt)
bad-sym
