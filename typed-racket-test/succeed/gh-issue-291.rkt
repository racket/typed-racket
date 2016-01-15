#lang typed/racket

;; Test inheritance with define-struct/exec

(define-struct/exec sA ([x : Number] [y : Number])
                    [(λ (self v) 1) : (→ sA Any Any)])

(define-struct/exec (sB sA) ([z : String])
                    [(λ (self v) 2) : (→ sB Any Any)])

(sB 1 2 "c")
(λ ([x : sB]) (ann x sA))
