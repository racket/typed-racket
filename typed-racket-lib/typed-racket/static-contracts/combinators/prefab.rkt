#lang racket/base

;; Static contract for prefab/c.

(require "../../utils/utils.rkt"
         "../structures.rkt"
         "../constraints.rkt"
         "../../utils/prefab.rkt"
         racket/match
         (contract-req)
         (for-template racket/base "../../utils/prefab-c.rkt")
         (for-syntax racket/base syntax/parse))

 

(provide prefab/sc:)

(provide/cond-contract
 [prefab/sc (prefab-key? (listof static-contract?) . -> . static-contract?)])

(struct prefab-combinator combinator (key field-mutability)
  #:transparent
  #:property prop:combinator-name "prefab/sc"
  #:methods gen:sc
    [(define (sc-map v f)
       (match-define (prefab-combinator args key field-mutability) v)
       (prefab-combinator
        (map (λ (a mut?) (f a (if mut? 'invariant 'covariant))) args field-mutability)
        key
        field-mutability))
     (define (sc-traverse v f)
       (match-define (prefab-combinator args key field-mutability) v)
       (for-each (λ (a mut?) (f a (if mut? 'invariant 'covariant))) args field-mutability)
       (void))
     (define (sc->contract v f)
       (match-define (prefab-combinator args key _) v)
       #`(prefab/c (quote #,(abbreviate-prefab-key key)) #,@(map f args)))
     (define (sc->constraints v f)
       (match-define (prefab-combinator args _ field-mutability) v)
       (merge-restricts* (if (ormap values field-mutability) 'chaperone 'flat)
                         (map (λ (a mut?)
                                (if (not mut?)
                                    (add-constraint (f a) 'chaperone)
                                    (f a)))
                              args
                              field-mutability)))])

(define (prefab/sc key fields)
  (prefab-combinator fields key (prefab-key->field-mutability key)))


(define-match-expander prefab/sc:
  (syntax-parser
    [(_ name fields)
     #'(prefab-combinator fields name _)]))

