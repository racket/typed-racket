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

(provide check-sub-signatures?
         valid-signatures?
         flatten-sigs)

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

(define (flatten-sigs sig)
  (if sig
      (cons sig
            (flatten-sigs (Signature-extends sig)))
      null))


;; valid-sigs : (listof Signature) -> boolean
;; returns true iff the list of signatures contains no duplicates under
;; extension
(define (valid-signatures? sigs)
  (define sig-ids (apply append (map signature-extensions sigs)))
  (distinct? sig-ids))

;; distinct? : (listof id) -> boolean?
;; returns true iff the signature ids are all distinct
(define (distinct? sigs)
  (or (empty? sigs)
      (and (not (member (first sigs) (rest sigs) free-identifier=?))
           (distinct? (rest sigs)))))
