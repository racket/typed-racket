#;
(exn-pred #rx"type mismatch")
#lang typed/racket

(define-signature x-sig ([x : Integer]))
(define-signature x-sub extends x-sig ([xx : Integer]))

(define u (unit (import) (export x-sig) (define x 1)))

(define-values/invoke-unit u (import) (export x-sub))
