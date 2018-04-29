#lang typed/racket/base

;; neither id should be bound in t/r lang

(require syntax/macro-testing typed/rackunit)

(check-exn #rx"unbound identifier"
  (lambda () (convert-compile-time-error with-type-shallow)))

(check-exn #rx"unbound identifier"
  (lambda () (convert-compile-time-error with-type-optional)))

