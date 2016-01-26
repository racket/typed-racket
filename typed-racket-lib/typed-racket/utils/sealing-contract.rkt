#lang racket/base

;; This module provides a sealing->/c which operates like the parametric->/c
;; contract except for classes and with partial sealing instead of fully
;; opaque sealing.

(require racket/class
         racket/contract
         racket/match
         (for-syntax racket/base
                     syntax/parse))

(provide sealing->/c)

(define-syntax (sealing->/c stx)
  (syntax-parse stx
    [(_ ?var:id [(?i:id ...) (?f:id ...) (?m:id ...)] ?c)
     #`(sealing-contract (quote #,(syntax->datum stx))
                         (quote ((?i ...) (?f ...) (?m ...)))
                         (λ (?var) ?c))]))

;; represents a sealing function contract
;; name     - a datum for the printed form of the contract
;; unsealed - init/field/method names left unsealed by sealers/unsealers
;; proc     - a procedure that constructs the whole contract
(struct sealing-contract (name unsealed proc)
        #:property prop:contract
        (build-contract-property
         #:name (λ (ctc) (sealing-contract-name ctc))
         #:stronger
         (λ (this that)
           (cond
            [(sealing-contract? that)
             (define this-unsealed (sealing-contract-unsealed this))
             (define that-unsealed (sealing-contract-unsealed that))
             (match-define (list this-inits this-fields this-methods) this-unsealed)
             (match-define (list that-inits that-fields that-methods) that-unsealed)
             (define (that-subset-of-this? this that)
               (for/and ([that-member (in-list that)])
                 (member that-member this)))
             (that-subset-of-this? this-inits that-inits)
             (that-subset-of-this? this-fields that-fields)
             (that-subset-of-this? this-methods that-methods)
             (cond [that-subset-of-this?
                    (define sealer/unsealer
                      (seal/unseal (gensym) #t that-unsealed))
                    ;; see if the instantiated contract is stronger
                    (contract-stronger? ((sealing-contract-proc this)
                                         sealer/unsealer)
                                        ((sealing-contract-proc that)
                                         sealer/unsealer))]
                   [else #f])]
            [else #f]))
         #:late-neg-projection
         (λ (ctc)
           (define unsealed (sealing-contract-unsealed ctc))
           (define body (sealing-contract-proc ctc))
           (λ (blame)
             (define negative? (blame-swapped? blame))
             (define (make-seal-function orig-fun neg-party)
               (define sealing-key (gensym))
               (define sealer/unsealer
                 (seal/unseal sealing-key negative? unsealed))
               (define body-proj
                 (contract-late-neg-projection (body sealer/unsealer)))
               ((body-proj blame) orig-fun neg-party))
             (λ (val neg-party)
               (unless (procedure? val)
                 (raise-blame-error
                  blame #:missing-party neg-party val
                  '(expected "a procedure" given: "~e") val))
               (define blame+neg-party (cons blame neg-party))
               ;; ok to return an unrelated function since this
               ;; is an impersonator contract
               (make-keyword-procedure (λ (kws kw-args . rst)
                                         (with-contract-continuation-mark
                                          blame+neg-party
                                          (keyword-apply (make-seal-function val neg-party)
                                                         kws kw-args rst)))))))))

;; represents a contract for each polymorphic seal/unseal corresponding
;; to a variable
;; key       - a fresh symbol used as a key
;; positive? - whether the contract starts in positive/negative
;; unsealed - names that are to be unsealed by sealers/unsealers
(struct seal/unseal (key positive? unsealed)
        #:property prop:contract
        (build-contract-property
         #:name (λ (ctc)
                  (if positive?
                      `(seal/c ,(seal/unseal-key ctc)
                               ,(seal/unseal-unsealed ctc))
                      '(unseal/c ,(seal/unseal-key ctc))))
         #:stronger (λ (this that) (eq? this that))
         #:late-neg-projection
         (λ (ctc)
           (define sealing-key (seal/unseal-key ctc))
           (define unsealed (seal/unseal-unsealed ctc))
           (λ (blame)
             (λ (val neg-party)
               (with-contract-continuation-mark
                (cons blame neg-party)
                (unless (class? val)
                  (raise-blame-error
                   blame #:missing-party neg-party val
                   '(expected: "a class" given: "~e") val))
                (match-define (list init field method) unsealed)
                (if (equal? (blame-original? blame)
                            (seal/unseal-positive? ctc))
                    (class-seal val sealing-key init field method
                                (inst-err val blame neg-party)
                                (subclass-err val blame neg-party))
                    (class-unseal val sealing-key
                                  (unseal-err val blame neg-party)))))))))

;; error functions for use with class-seal, class-unseal
(define ((inst-err val blame neg-party) cls)
  (raise-blame-error
   blame #:missing-party neg-party val
   "cannot instantiated a row polymorphic class"))

(define ((subclass-err val blame neg-party) cls names)
  (raise-blame-error
   blame #:missing-party neg-party val
   "cannot subclass a row polymorphic class with disallowed members ~a"
   names))

(define ((unseal-err val blame neg-party) cls)
  (raise-blame-error
   blame #:missing-party neg-party val
   '(expected "matching row polymorphic class"
     given: "an unrelated class")))
