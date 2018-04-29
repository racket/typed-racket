#lang typed/racket/base/optional

;; A symbol type should generate a contract that accepts the same symbol

(module uuu racket/base
  (define FOUNDING 'FOUNDING)
  (provide FOUNDING))

(require/typed/provide 'uuu
  (FOUNDING 'FOUNDING))
