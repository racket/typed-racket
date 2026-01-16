#;
(exn-pred #rx"Macro.*from typed module used in untyped code")
#lang racket/load

;; Test that macros from typed modules produce the correct error message
;; when used in untyped code (related to #1469)

(module typed-mod typed/racket/base
  (provide my-macro)
  (define-syntax-rule (my-macro x) x))

(module untyped-mod racket/base
  (require 'typed-mod)
  (my-macro 42))
