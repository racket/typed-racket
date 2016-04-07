#;
(exn-pred #rx"struct:: expected one of these literals")

#lang typed/racket

;; Check that #:methods is ruled out
(struct: foo ([a : Integer]) #:methods gen:dict [])

