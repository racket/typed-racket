#lang racket/base

;; This module provides object contracts that prevent access
;; to typed methods and fields that are left out of an object type
;; via subtyping.
;;
;; These correspond to `object/c` and `OG` guards from
;; "Gradual Typing for First-Class Classes"
;;
;; Methods:
;; --------
;; The contract allows untyped contexts to call untyped methods on
;; protected objects freely. Typed methods without explicit contracts
;; are in the opaque part and cannot be called from any context after
;; the contract is applied.
;;
;; Allowing untyped-untyped calls (without any contract) is a liberalization
;; of the contract semantics, but it should not cause problems because
;; typed code always protects its own invariants via contracted imports or
;; exports (and typed methods are not callable without contracts).
;;
;; Fields:
;; -------
;; Fields are blocked from access without a contract in all cases. Allowing
;; untyped-untyped access may be ok, but could produce unhelpful blame.
;; Allowing typed-typed access is not ok unless it's restricted to identical
;; blame parties (e.g., a particular module).

(require racket/class
         racket/match
         racket/contract/base
         racket/contract/combinator
         "typed-method-property.rkt"
         (for-syntax racket/base
                     syntax/parse))

(provide object/c-opaque)

(begin-for-syntax
 (define-syntax-class object/c-clause
   #:attributes (method-names method-ctcs field-names field-ctcs)
   (pattern ((~literal field) [name:id ctc:expr] ...)
            #:with field-names #'(list (quote name) ...)
            #:with field-ctcs #'(list ctc ...)
            #:with method-names #'null
            #:with method-ctcs #'null)
   (pattern [name:id ctc:expr]
            #:with field-names #'null
            #:with field-ctcs #'null
            #:with method-names #'(list (quote name))
            #:with method-ctcs #'(list ctc))))

(define-syntax (object/c-opaque stx)
  (syntax-parse stx
   [(_ ?clause:object/c-clause ...)
    (syntax/loc stx
      (object/c ?clause ...
                #:opaque-except typed-method?
                #:opaque-fields #t
                #:do-not-check-class-field-accessor-or-mutator-access))]))
