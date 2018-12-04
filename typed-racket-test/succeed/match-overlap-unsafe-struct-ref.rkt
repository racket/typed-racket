#lang typed/racket/base
;; from GH Issue #789
(require racket/match)
;; NB: I left StrC out of ExprC! Oops!
(define-type ExprC (U NumC Primop ))
(struct NumC ([n : Real]) #:transparent)
(struct StrC ([s : String]) #:transparent)

(define-type Primop (Real Real -> Real))


(define (type-check [e : ExprC]) : Real
  (match e
    [(NumC n) 12]
    [(StrC s) 13]   
    [_ 14]))