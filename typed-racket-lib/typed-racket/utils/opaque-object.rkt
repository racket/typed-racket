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

;; projection for base-object/c-opaque
(define ((object/c-opaque-late-neg-proj ctc) blame)
  (λ (obj neg-party)
    (match-define (base-object/c-opaque
                   base-ctc
                   methods method-ctcs
                   fields field-ctcs)
                  ctc)
    (when (not (object? obj))
      (raise-blame-error blame #:missing-party neg-party obj "expected an object got ~a" obj))
    (define actual-fields (field-names obj))
    (define actual-methods
      (interface->method-names (object-interface obj)))
    (define remaining-fields
      (remove* fields actual-fields))
    (define remaining-methods
      (remove* methods actual-methods))
    (define guard/c
      (dynamic-object/c (append methods remaining-methods)
                        (append method-ctcs
                                (for/list ([m remaining-methods])
                                  (restrict-typed->/c)))
                        (append fields remaining-fields)
                        (append field-ctcs
                                (for/list ([m remaining-fields])
                                  (restrict-typed-field/c obj m)))))
    ;; FIXME: this is a bit sketchy because we have to construct
    ;;        a contract that depends on the actual object that we got
    ;;        since we don't know its methods beforehand
    (((contract-late-neg-projection guard/c) blame) obj neg-party)))

(struct base-object/c-opaque
  (obj/c ; keep a copy of the normal object/c for first-order checks
   method-names method-ctcs field-names field-ctcs)
  #:property prop:contract
  (build-contract-property
   #:first-order (λ (ctc)
                   (define obj/c (base-object/c-opaque-obj/c ctc))
                   (λ (val)
                     (contract-first-order-passes? obj/c val)))
   #:late-neg-projection object/c-opaque-late-neg-proj))

(begin-for-syntax
 (define-syntax-class object/c-clause
   #:attributes (method-names method-ctcs field-names field-ctcs)
   (pattern (field [name:id ctc:expr] ...)
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
      (let ([names  (append ?clause.method-names ...)]
            [ctcs   (append ?clause.method-ctcs ...)]
            [fnames (append ?clause.field-names ...)]
            [fctcs  (append ?clause.field-ctcs ...)])
        (base-object/c-opaque
         (dynamic-object/c names ctcs fnames fctcs)
         names ctcs fnames fctcs)))]))

;; This contract combinator prevents the method call if the target
;; method is typed (assuming that the caller is untyped or the receiving
;; object went through untyped code)
(define (((restrict-typed->-late-neg-projection ctc) blame) val neg-party)
  (define blame+neg-party (cons blame neg-party))
  (chaperone-procedure val
                       (make-keyword-procedure
                        (λ (_ kw-args . rst)
                          (with-contract-continuation-mark
                           blame+neg-party
                           (when (typed-method? val)
                             (raise-blame-error (blame-swap blame) val #:missing-party neg-party
                                                "cannot call uncontracted typed method"))
                           (apply values kw-args rst)))
                        (λ args
                          (with-contract-continuation-mark
                           blame+neg-party
                           (when (typed-method? val)
                             (raise-blame-error (blame-swap blame) val #:missing-party neg-party
                                                "cannot call uncontracted typed method"))
                           (apply values args))))))

(struct restrict-typed->/c ()
        #:property prop:chaperone-contract
        (build-chaperone-contract-property
         #:name (λ (ctc) '<hidden-method>) ; FIXME
         #:late-neg-projection restrict-typed->-late-neg-projection))

(define (restrict-typed-field-late-neg-proj ctc)
  (define name (restrict-typed-field/c-name ctc))
  (λ (*blame)
    (define blame
      ;; Blame has been swapped if this is for a set-field!, in which case
      ;; the blame matches the original negative party. Otherwise we want
      ;; to swap to blame negative.
      (if (blame-swapped? *blame)
          *blame
          (blame-swap *blame)))
    (λ (val neg-party)
      (raise-blame-error
       blame val #:missing-party neg-party
       "cannot read or write field hidden by Typed Racket"))))

(struct restrict-typed-field/c (obj name)
        #:property prop:flat-contract
        (build-flat-contract-property
         #:name (λ (ctc) '<hidden-field>) ; FIXME
         #:late-neg-projection restrict-typed-field-late-neg-proj))
