#;
(exn-pred (regexp-quote "type mismatch\n  expected: (Unit (import) (export x-sig) (init-depend) AnyValues)\n  given: (Unit (import) (export) (init-depend) Void)"))
#lang typed/racket

(define-signature x-sig ([x : Integer]))

(define-values/invoke-unit (unit (import) (export)) (import) (export x-sig))
