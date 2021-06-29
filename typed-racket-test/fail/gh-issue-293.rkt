#;
(exn-pred #rx"typed-racket-test/fail/gh-issue-293\\.rkt:5:41.*\n.*expected: Procedure.*")
#lang typed/racket

(define-struct/exec foo ([x : Integer]) ["foo" : String])
