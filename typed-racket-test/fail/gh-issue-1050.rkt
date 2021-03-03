#;
(exn-pred exn:fail:syntax? #rx"type-check: type name used out of context.*? type: Exp")
#lang typed/racket/base

(module typed1 typed/racket/base
  (provide (except-out (all-defined-out) make-exp))
  (struct Exp () #:constructor-name make-exp))

(require 'typed1)
(Exp)
