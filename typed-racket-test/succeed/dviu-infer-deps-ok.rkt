#lang typed/racket

(define-signature s^ ([a : Integer]))
(define-signature t^ ([b : Integer]))
(define-unit u@
  (import s^)
  (export t^)
  (init-depend s^)
  (define b a))
(define-unit v@
  (import)
  (export s^)
  (define a 2))
(define-values/invoke-unit/infer (export) (link v@ u@))
