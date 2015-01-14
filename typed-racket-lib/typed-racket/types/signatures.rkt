#lang racket/base

;; This module provides helper functions for typed signatures needed for subtyping

(require "../utils/utils.rkt"
         (env signature-env)
        (rep type-rep)
        ; (private parse-type)
         syntax/parse
         racket/list
         (only-in racket/set subset?)
         (for-template racket/base
                       (typecheck internal-forms)))

(provide check-sub-signatures?)

;; check-subsignatures? : (listof Signature) (listof Signature) -> boolean?
;; checks that the first list of signatures is a valid "subtype"/extensions
;; of the second list of signatures
(define (check-sub-signatures? sub-sigs sup-sigs)
  (define sub-exts (append-map signature-extensions sub-sigs))
  (define sup-exts (append-map signature-extensions sup-sigs))
  ;(printf "sub-exts: ~a\n" sub-exts)
  ;(printf "sup-exts: ~a\n" sup-exts)
  (define (subset? s1 s2)
    (andmap
     (lambda (s1-elem) (member s1-elem s2 free-identifier=?))
     s1))
  
  (subset? sup-exts sub-exts))

;; signature-extensions : (or/c #f Signature) -> (listof identifier?)
;; returns the list (chain) of names of each signature that
;; the given signature extends including itself
;; returns '() when given #f
(define (signature-extensions sig)
  (if sig
      (cons (Signature-name sig)
            (signature-extensions (Signature-extends sig)))
      null))
