#;
(exn-pred #rx"type mismatch")
#lang typed/racket

(define-signature bad^ ())
(define bad
  (let ()
    (define-signature bad^ ())
    (unit (import bad^) (export))))

(invoke-unit bad (import bad^))
