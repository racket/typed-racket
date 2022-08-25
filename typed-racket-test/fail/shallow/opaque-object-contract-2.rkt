#;
(exn-pred #rx"cannot read or write.*blaming:.*b\\)")
#lang racket/base

;; Ensure that opaque object contracts prevent access to fields
;; that should be hidden

(module a typed/racket/deep
  (provide o)
  (: o (Object))
  (define o (new (class object%
                   (super-new)
                   (field [x 0])))))

(module b typed/racket/shallow
  (require (submod ".." a))
  (let ((obj (cast o (Object (field [x Zero])))))
    (set-field! x obj 0) ;; hidden field, should fail
    (get-field x obj)))

(require 'b)
