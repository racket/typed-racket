#lang racket/base

;; This module provides helper functions for typed signatures needed for 
;; checking subtyping and parsing unit types

(require "../utils/utils.rkt"
         (env signature-env)
         (rep type-rep)
         syntax/parse
         syntax/id-set
         racket/list
         (only-in racket/set subset?)
         (for-template racket/base
                       (typecheck internal-forms)))

(provide check-sub-signatures?
         distinct-signatures?
         flatten-sigs
         check-imports/exports
         check-init-depends/imports)

;; (listof Signature?) procedure? -> (listof Signature?)
;; given a list of imported/exported signatures
;; and a parse-error procedure, ensures that the signatures are
;; valid in that there are no overlapping signatures
;; Returns the given list or raises an error
(define (check-imports/exports sigs error-proc)
  (unless (distinct-signatures? sigs) (error-proc))
  sigs)

;; (listof Signature?) (listof Signature?) procedure? -> (listof Signature?)
;; Ensures that the init-depends signatures are a subset of the import signatures
;; an equal? based subset check is the right thing, since each init-depend
;; must be equal to an imported signature
(define (check-init-depends/imports init-depends imports error-proc)
  (unless (subset? init-depends imports) (error-proc))
  init-depends)

;; check-sub-signatures? : (listof Signature) (listof Signature) -> boolean?
;; checks that the first list of signatures can be used to implement the second
;; list of signatures
(define (check-sub-signatures? sub-sigs sup-sigs)
  (define sub-exts (immutable-free-id-set (append-map signature-extensions sub-sigs)))
  (define sup-exts (immutable-free-id-set (append-map signature-extensions sup-sigs)))
  (free-id-subset? sup-exts sub-exts))

;; signature-extensions : (or/c #f identifier?) -> (listof identifier?)
;; returns the list (chain) of names of each signature that
;; the given signature extends including itself
;; returns '() when given #f
(define (signature-extensions sig*)
  (let ([sig (and sig* (if (Signature? sig*) sig* (lookup-signature sig*)))])
    (if sig
        (cons (Signature-name sig)
              (signature-extensions (Signature-extends sig)))
        null)))

(define (flatten-sigs sig*)
  (let ([sig (and sig* (if (Signature? sig*) sig* (lookup-signature sig*)))])
    (if sig
        (cons sig (flatten-sigs (Signature-extends sig)))
        null)))


;;  : (listof Signature) -> boolean
;; returns true iff the list of signatures contains no duplicates under
;; extension
(define (distinct-signatures? sigs)
  (define sig-ids (append-map signature-extensions sigs))
  (distinct-ids? sig-ids))

;; distinct-ids? : (listof id) -> boolean?
;; returns true iff the signature ids are all distinct
(define (distinct-ids? sigs)
  (not (check-duplicates sigs free-identifier=?)))
