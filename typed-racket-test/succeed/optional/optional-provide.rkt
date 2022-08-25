#lang racket/base

;; Test providing value to untyped

(module a typed/racket/base/optional
        (provide f)
        (define (f (x : (Boxof Integer)))
          (unbox x)))

(require 'a rackunit)
(check-exn exn:fail:contract?
  (lambda () (f 0)))
