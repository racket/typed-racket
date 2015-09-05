#;
(exn-pred (regexp-quote "type mismatch\n  expected: (Unit (import) (export) (init-depend) Any)\n  given: (Unit (import x-sig) (export) (init-depend) Integer)"))

#lang typed/racket

(define-signature x-sig ([x : Integer]))
(define-values/invoke-unit (unit (import x-sig) (export) x) (import) (export))
