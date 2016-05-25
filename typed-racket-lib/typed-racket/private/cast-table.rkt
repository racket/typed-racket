#lang racket/base

(provide cast-table-ref
         cast-table-set!)

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

;; cast-table-set! : Id Type-Stx -> Void
(define (cast-table-set! id type-stx)
  (free-id-table-set! cast-table id type-stx))

;; cast-table-ref : Id -> (U False Type-Stx)
(define (cast-table-ref id)
  (free-id-table-ref cast-table id #f))
