#;
(exn-pred exn:fail:contract? #rx"sym")
#lang typed/racket/base/shallow

(module uuu racket/base
  (provide bad-sym)
  (define bad-sym #f))

(module opt typed/racket/base/optional
  (require/typed (submod ".." uuu)
    ((bad-sym -bad-sym) Symbol))
  (define bad-sym -bad-sym)
  (provide bad-sym))

(require 'opt)
bad-sym
