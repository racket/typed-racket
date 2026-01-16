#;
(exn-pred #rx"Macro.*from typed module used in shallow-typed code")
#lang racket/load

;; Test that macros from typed modules produce the correct error message
;; when used in shallow-typed code (fixes #1469)

(module typed-mod typed/racket/base
  (provide my-macro)
  (define-syntax-rule (my-macro x) x))

(module shallow-mod typed/racket/base/shallow
  (require 'typed-mod)
  (my-macro 42))
