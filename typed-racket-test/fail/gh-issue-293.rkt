#;
(exn-pred #rx"type mismatch.*expected: Procedure.*given: String")
#lang typed/racket

(define-struct/exec foo ([x : Integer]) ["foo" : String])
