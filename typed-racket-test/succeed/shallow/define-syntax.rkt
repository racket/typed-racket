#lang typed/racket/base/shallow

;; Test that a simple macro works in locally-defensive code

(define-syntax-rule (yo lo)
  (+ lo lo))

(: f (-> Integer Integer))
(define (f x)
  (yo (yo x)))

(f 42)
