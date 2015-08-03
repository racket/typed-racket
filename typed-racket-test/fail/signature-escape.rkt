#;
(exn-pred #rx"signature escapes")
#lang typed/racket

(define bad
  (let ()
    (define-signature bad^ ())
    (unit (import) (export bad^))))
