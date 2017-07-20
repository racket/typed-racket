#lang racket/base

(provide cast-table-ref
         cast-table-add!)

(require syntax/id-table)

;; A module that helps store information about the original types of casted
;; expressions.
;;
;; Casts in Typed Racket must generate two contracts. One from typed to untyped,
;; and another from untyped to typed. The contract from typed to untyped is
;; generated based on the existing type of the expression, which must be stored
;; in this table so that it can be looked up later in the contract-generation
;; pass.

(define cast-table (make-free-id-table))

;; cast-table-add! : Id Type-Stx -> Void
(define (cast-table-add! id type-stx)
  (free-id-table-update! cast-table id
    (λ (lst) (cons type-stx lst))
    (λ () '())))

;; cast-table-ref : Id -> (U False (Listof Type-Stx))
(define (cast-table-ref id)
  (free-id-table-ref cast-table id #f))
