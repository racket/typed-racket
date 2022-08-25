#lang typed/racket/shallow

;; Identifier imported from untyped-contract stuck with refined type
;;  in a shallow context.
;; Need to abide by that refined type

(require "untyped-contract-aux/untyped.rkt")

(define (g (z : (U Symbol String))) : Void
  ;; (f z) = type error
  (f (assert z symbol?)))

