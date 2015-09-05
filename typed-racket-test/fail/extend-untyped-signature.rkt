#;
(exn-pred #rx"signature definition extends untyped signature")
#lang racket

(module ut1 racket
  (provide a^)
  (define-signature a^ (a)))

(module t1 typed/racket
  (require (submod ".." ut1))
  (define-signature a-sub^ extends a^ ())
  (unit (import a-sub^) (export) a))
  