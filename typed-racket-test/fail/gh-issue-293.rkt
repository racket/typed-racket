#;
(exn-pred "expected: Procedure")
#lang typed/racket

(define-struct/exec foo ([x : Integer]) ["foo" : String])
