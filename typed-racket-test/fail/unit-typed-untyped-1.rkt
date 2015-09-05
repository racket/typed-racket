#;
(exn-pred #rx"u: broke its own contract\n  promised: Integer")
#lang racket

(module untyped racket
  (provide u)
  (define u (unit (import) (export) "not an integer")))

(module typed typed/racket
  (require/typed (submod ".." untyped)
    [u (Unit (import) (export) Integer)])
  (invoke-unit u))

(require 'typed)
