#;
(exn-pred #px"expected an identifier")
#lang typed/racket

;; Test that #{x : T} rejects function applications
;; Identifiers and literals (including quoted) are allowed
;; But function applications like (+ 1 2) should fail

(define x #{(+ 1 2) : Number})  ; Should fail - (+ 1 2) is a function application, not an identifier
