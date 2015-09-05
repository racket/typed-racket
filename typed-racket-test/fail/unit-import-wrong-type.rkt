#;
(exn-pred #rx"Type Checker: type mismatch\n  expected: Integer\n  given: String")
#lang typed/racket/base
(require typed/racket/unit)


(define-signature x^ ([x : String]))

(unit (import x^)
      (export )
      (: y Integer)
      (define y x)
      y)
