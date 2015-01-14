#;
(exn:pred #rx"type mismatch")

#lang typed/racket

(define-signature x-sig ([x : Integer]))
(define-values/invoke-unit (unit (import x-sig) (export) x) (import) (export))
