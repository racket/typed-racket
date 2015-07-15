#lang racket/base

(require (for-syntax racket/base))

(provide ;; for use in ~literal clauses
         unit-internal)

(define-syntax (unit-internal stx)
  (raise-syntax-error 'unit "should only be used internally"))
