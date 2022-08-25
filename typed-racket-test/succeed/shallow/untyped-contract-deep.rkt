#lang typed/racket

;; Identifier imported from untyped-contract should have original type,
;;  not the type for the refined contract

(require "untyped-contract-aux/untyped.rkt")

(define (g (z : (U Symbol String))) : Void
  (f z))
