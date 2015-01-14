#;
(exn-pred #rx"contract does not list initialization dependency a\\^")

#lang racket

(module untyped racket
  (provide (all-defined-out))
  (define-signature a^ (a))
  (define-unit u
    (import a^)
    (export)
    (init-depend a^)
    a))

(module typed typed/racket
  (require/typed (submod ".." untyped)
    [#:signature a^ ([a : Any])]
    [u (Unit (import a^) (export) Any)]))

(require 'typed)
