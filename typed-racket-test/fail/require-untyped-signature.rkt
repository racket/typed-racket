#;
(exn-pred (regexp-quote "use of untyped signature in typed code"))
#lang racket

(module A racket
  (provide a)
  (define-signature a ()))

(module B typed/racket
  (require (submod ".." A))
  (define-unit u
    (import a)
    (export)))