#;
(exn-pred (regexp-quote "type mismatch\n  expected: (Unit (import bad^) (export) (init-depend bad^) AnyValues)\n  given: (Unit (import bad^) (export) (init-depend) Void)\n  in: (invoke-unit bad (import bad^))"))
#lang typed/racket

;; The full error message for this prorgam is
;; Type Checker: type mismatch
;; expected: (Unit (import bad^) (export) (init-depend bad^) AnyValues)
;; given: (Unit (import bad^) (export) (init-depend) Void)
;; in: (invoke-unit bad (import bad^))


(define-signature bad^ ())
(define bad
  (let ()
    (define-signature bad^ ())
    (unit (import bad^) (export))))

(invoke-unit bad (import bad^))
