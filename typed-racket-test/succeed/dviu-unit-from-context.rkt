#lang typed/racket

(define-signature yz-sig ([y : Integer] [z : Integer]))
(define y 1)
(define z 10)
(define-values/invoke-unit (unit-from-context yz-sig)
  (import)
  (export (prefix x: yz-sig)))
(+ x:y x:z)

