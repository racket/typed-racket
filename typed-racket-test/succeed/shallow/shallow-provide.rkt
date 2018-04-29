#lang racket/base

;; Test providing value to untyped

(module a typed/racket/base/shallow
        (provide f)
        (define (f (x : (Boxof Integer)))
          (unbox x)))

(require 'a rackunit)
(check-exn #rx"shape-check"
  (lambda () (f 0)))
