#lang racket/base

;; This module provides helper functions for dealing with
;; unit and signature types

(require "../utils/utils.rkt"
         (rep type-rep rep-utils)
         (utils tc-utils)
         racket/list
         racket/match)

(provide signatures-escape?)


;; (Listof Signature) Type -> Boolean
;; Returns true if the type contains an instance of one
;; of the signatures
;; A simple escape analysis for use in tc/letrec-values
;; to ensure that signatures may not be referenced outside
;; of their scope of definition
(define (signatures-escape? sigs type)
  (define escaping-names (map Signature-name sigs))
  (define (check-signatures type)
    (type-case
     (#:Type check-signatures
      #:Filter (sub-f check-signatures)
      #:Object (sub-o check-signatures))
     type
     [#:Signature name extends mapping
      (if (member name escaping-names free-identifier=?)
          (tc-error/fields "signature escapes let body"
                           "signature" (syntax-e name))
          type)]))
  (check-signatures type))
