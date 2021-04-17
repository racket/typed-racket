#;
(exn-pred #rx"Exp: identifier for static struct-type information cannot be used as an expression")
#lang typed/racket/base

(provide (except-out (all-defined-out) make-exp))
(struct exp () #:type-name Exp #:constructor-name make-exp)

(Exp)
