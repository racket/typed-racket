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
         (only-in racket/private/class-c-old
                  base-object/c? build-object/c-type-name object/c-width-subtype?
                  object/c-common-methods-stronger? object/c-common-fields-stronger?)
         racket/match
         racket/contract/base
         racket/contract/combinator
         "typed-method-property.rkt"
         (for-syntax racket/base
                     syntax/parse))

(provide object/c-opaque)

(module+ for-testing
  (provide restrict-typed->/c
           restrict-typed-field/c))

;; projection for base-object/c-opaque
(define ((object/c-opaque-late-neg-proj ctc) blame)
  (match-define (base-object/c-opaque
                 base-ctc
                 methods method-ctcs
                 fields field-ctcs)
    ctc)
  (define guard/c (dynamic-object/c methods method-ctcs fields field-ctcs))
  (define guard/c-proj ((contract-late-neg-projection guard/c) blame))
  (λ (obj neg-party)
    (when (not (object? obj))
      (raise-blame-error blame #:missing-party neg-party obj "expected an object got ~a" obj))
    (define actual-fields (field-names obj))
    (define actual-methods
      (interface->method-names (object-interface obj)))
    (define remaining-fields
      (remove* fields actual-fields))
    (define remaining-methods
      (remove* methods actual-methods))
    (cond
      [(and (null? remaining-methods) (null? remaining-fields))
       (guard/c-proj obj neg-party)]
      [else
       (define restrict-guard/c
         (dynamic-object/c remaining-methods
                           (for/list ([m (in-list remaining-methods)])
                             (restrict-typed->/c m))
                           remaining-fields
                           (for/list ([m (in-list remaining-fields)])
                             (restrict-typed-field/c m))))
       ;; FIXME: this is a bit sketchy because we have to construct
       ;;        a contract that depends on the actual object that we got
       ;;        since we don't know its methods beforehand
       (((contract-late-neg-projection restrict-guard/c) blame)
        (guard/c-proj obj neg-party)
        neg-party)])))

(define (object/c-opaque-name ctc)
  (build-object/c-type-name 'object/c-opaque
                            (base-object/c-opaque-method-names ctc)
                            (base-object/c-opaque-method-ctcs ctc)
                            (base-object/c-opaque-field-names ctc)
                            (base-object/c-opaque-field-ctcs ctc)))

;; Similar to object/c-stronger, but without width subtyping.
;; (Intuition: unspecified fields are guarded by the strongest possible contract)
;; An opaque object contract `this` is stronger than `that` when:
;; - `that` is an opaque contract
;;   and `this` specifies at most the same members as `that`
;;   and `this` has stronger contracts on all members
;; - `that` is an object/c contract
;;   and `this` has stronger contracts on their common members
(define (object/c-opaque-stronger? this that)
  (define that-opaque? (base-object/c-opaque? that))
  (cond
   [(or that-opaque?
        (base-object/c? that))
    (define this-ctc (base-object/c-opaque-obj/c this))
    (define that-ctc (if that-opaque? (base-object/c-opaque-obj/c that) that))
    (and
      (if that-opaque?
        ;; then members of `this` should be a SUBSET of members of `that`
        (object/c-width-subtype? that-ctc this-ctc)
        #t)
      (object/c-common-fields-stronger? this-ctc that-ctc)
      (object/c-common-methods-stronger? this-ctc that-ctc)
      #t)]
   [else #f]))

;; An `object/c-opaque` contract is equivalent to another `object/c-opaque`
;;  contract that has the same fields+methods and the same contracts on them.
(define (object/c-opaque-equivalent? this that)
  (and (base-object/c-opaque? that)
       (contract-equivalent? (base-object/c-opaque-obj/c this)
                             (base-object/c-opaque-obj/c that))))

(struct base-object/c-opaque
  (obj/c ; keep a copy of the normal object/c for first-order and stronger checks
   method-names method-ctcs field-names field-ctcs)
  #:property prop:contract
  (build-contract-property
   #:stronger object/c-opaque-stronger?
   #:equivalent object/c-opaque-equivalent?
   #:name object/c-opaque-name
   #:first-order (λ (ctc)
                   (define obj/c (base-object/c-opaque-obj/c ctc))
                   (λ (val)
                     (contract-first-order-passes? obj/c val)))
   #:late-neg-projection object/c-opaque-late-neg-proj))

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
  (cond
    [(typed-method? val)
     (chaperone-procedure val
                          (make-keyword-procedure
                           (λ (_ kw-args . rst)
                             (raise-blame-error (blame-swap blame) val #:missing-party neg-party
                                                "cannot call uncontracted typed method"))
                           (λ args
                             (raise-blame-error (blame-swap blame) val #:missing-party neg-party
                                                "cannot call uncontracted typed method"))))]
    [else val]))

;; Returns original method name
(define (restrict-typed->-name ctc)
  (define name (restrict-typed->/c-name ctc))
  (build-compound-type-name 'restrict-typed->/c name))

(define (restrict-typed->/c-equivalent? this that)
  (and (restrict-typed->/c? that)
       (eq? (restrict-typed->/c-name this)
            (restrict-typed->/c-name that))))

(struct restrict-typed->/c (name)
        #:property prop:chaperone-contract
        (build-chaperone-contract-property
         #:name restrict-typed->-name
         #:stronger restrict-typed->/c-equivalent?
         #:equivalent restrict-typed->/c-equivalent?
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

(define (restrict-typed-field-name ctc)
  (define name (restrict-typed-field/c-name ctc))
  (build-compound-type-name 'restrict-typed-field/c name))

(define (restrict-typed-field-equivalent? this that)
  (and (restrict-typed-field/c? that)
       (equal? (restrict-typed-field/c-name this)
               (restrict-typed-field/c-name that))))

(struct restrict-typed-field/c (name)
        #:property prop:flat-contract
        (build-flat-contract-property
         #:name restrict-typed-field-name
         #:stronger restrict-typed-field-equivalent?
         #:equivalent restrict-typed-field-equivalent?
         #:late-neg-projection restrict-typed-field-late-neg-proj))
