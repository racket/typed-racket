#;
(exn-pred #rx"cannot read or write.*blaming:.*b\\)")
#lang racket/base

;; Ensure that opaque object contracts prevent access to fields
;; that should be hidden from untyped code

(require racket/class)

(module a typed/racket
  (provide o)
  (: o (Object))
  (define o (new (class object%
                   (super-new)
                   (field [x 0])))))

(module b racket
  (require (submod ".." a))
  (set-field! x o "wrong type")
  (get-field x o))

(require 'b)
