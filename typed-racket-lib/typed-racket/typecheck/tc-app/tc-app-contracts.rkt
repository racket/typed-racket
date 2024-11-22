#lang racket/unit

;; This module provides custom type-checking rules for the expansion
;; of contracted values

(require (for-template racket/base
                       ;; shift -1 because it's provided +1
                       racket/contract/private/provide)
         syntax/parse
         (only-in typed-racket/types/type-table add-typeof-expr)
         (only-in typed-racket/types/tc-result ret)
         "../../utils/utils.rkt"
         "../signatures.rkt"
         "signatures.rkt"
         "utils.rkt")

(import tc-expr^)
(export tc-app-contracts^)

(define-tc/app-syntax-class (tc/app-contracts expected)
  (pattern (ctc-id:id blame e ...)
    ;; check that this is an application from the contract system
    #:when (contract-neg-party-property #'ctc-id)
    (check-contract #'ctc-id #'(e ...) expected)))

;; Assume that the contracted thing is of the same type the type
;; environment assigned to the exported identifier. Note that this
;; is only sound if the contract is a chaperone contract, so don't
;; put things in the base type environment if they have impersonator
;; contracts installed.
(define (check-contract orig-value-id other-args expected)
  (define ctc-id (contract-rename-id-property orig-value-id))
  (define ctc-ty (tc-expr/t ctc-id))
  (add-typeof-expr orig-value-id (ret ctc-ty))
  (tc-expr/check #`(#%plain-app
                    #,ctc-id
                    . #,other-args)
                 expected))

