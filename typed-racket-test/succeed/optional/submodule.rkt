#lang typed/racket/base/optional

;; Test importing a flat value

(module u racket/base
  (define x 3)
  (provide x))

(require/typed 'u
  (x Byte))

(+ x 1)
