#;
(exn-pred #rx"unit depends on initialization of later unit")
#lang typed/racket

;; This program errors at compile time, but the check is from
;; the untyped implementation of define-values/invoke-unit/infer

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
(define-values/invoke-unit/infer (export) (link u@ v@))
