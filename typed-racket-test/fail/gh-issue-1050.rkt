#;
(exn-pred #rx"Exp: identifier for static struct-type information cannot be used as an expression")
#lang typed/racket/base

(module typed1 typed/racket/base
  (provide (except-out (all-defined-out) make-exp))
  (struct Exp () #:constructor-name make-exp))

(require 'typed1)
(Exp)
