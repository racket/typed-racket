#lang typed/racket

(define-signature a1^ ([a1 : Integer]))
(define-signature a2^ extends a1^ ([a2 : Integer]))
(define-signature b^ ([b : Integer]))

(define-unit u1 (import) (export a2^) (define a1 6) (define a2 7))
(define-unit u2 (import a1^) (export b^) (define b (+ a1 11)))


(define-values/invoke-unit/infer (export) (link u1 u2))