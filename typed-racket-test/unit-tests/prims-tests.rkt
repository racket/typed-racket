#lang racket/base

;; Tests for Typed Racket primitive macros

(require "test-utils.rkt"
         (base-env base-types-extra)
         (base-env prims)
         rackunit
         syntax/macro-testing)

(provide tests)
(gen-test-main)

(define-syntax-rule (check-ok exp)
  (check-not-exn (λ () (convert-compile-time-error exp))))
(define-syntax-rule (check-bad exp regex)
  (check-exn regex (λ () (convert-compile-time-error exp))))

;; These tests don't run the type-checker, they just correct expansion
;; and error checking in primitive macros
(define tests
  (test-suite "Prims tests"
    (check-bad (let () (: 2 Zero) 'dummy)
               #rx"expected identifier")
    (check-bad (let () (: x) 'dummy)
               #rx"missing type")
    (check-bad (let () (: x Listof Any) 'dummy)
               #rx"too many types")
    (check-bad (let () (: x : Listof Any) 'dummy)
               #rx"too many types")
    (check-bad (let () (: x : -> String String) 'dummy)
               #rx"too many types")
    (check-ok (let () (: x : String -> String -> String)
                      (define x 0)
                      'dummy))))
