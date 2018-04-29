#;
(exn-pred exn:fail:contract? #rx"sym")
#lang typed/racket/base/shallow

(module uuu racket/base
  (provide bad-sym)
  (define bad-sym #f))

(module opt typed/racket/base/optional
  (provide sym)
  (require/typed (submod ".." uuu)
    (bad-sym Symbol))
  (define sym : Symbol bad-sym))

(require 'opt)
sym
