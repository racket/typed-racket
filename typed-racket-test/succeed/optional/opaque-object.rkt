#lang racket/base

;; Shallow does not make opaque contracts

(require racket/class)

(module a typed/racket/optional
  (provide o)
  (: o (Object))
  (define o
    (new
      (class object%
        (super-new)
        (field [x 0])
        (define/public (m) (void))))))

(module b racket
  (require (submod ".." a))
  (set-field! x o "wrong type")
  (get-field x o)
  (send o m))

(require 'b)
