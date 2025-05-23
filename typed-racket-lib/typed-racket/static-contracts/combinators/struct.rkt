#lang racket/base

;; Static contract for struct/c.

(require "../../utils/utils.rkt"
         "../structures.rkt" "../constraints.rkt"
         racket/match
         (contract-req)
         (for-template racket/base racket/contract/base "../../utils/struct-type-c.rkt")
         (for-syntax racket/base syntax/parse))



(provide
  struct/sc:
  struct-type/sc:)

(provide/cond-contract
 [struct/sc (identifier? boolean? (listof static-contract?) . -> . static-contract?)]
 ;; #f as argument indicates StructTypeTop, which should fail on
 ;; all reflective operations.
 [struct-type/sc (any/c . -> . static-contract?)])

(struct struct-combinator combinator (name mut?)
  #:transparent
  #:property prop:combinator-name "struct/sc"
  #:methods gen:sc
    [(define (sc-map v f)
       (match-define (struct-combinator args name mut?) v)
       (struct-combinator (map (λ (a) (f a (if mut? 'invariant 'covariant))) args) name mut?))
     (define (sc-traverse v f)
       (match-define (struct-combinator args name mut?) v)
       (for-each (λ (a) (f a (if mut? 'invariant 'covariant))) args)
       (void))
     (define (sc->contract v f)
       (match-define (struct-combinator args name _) v)
       #`(struct/c #,name #,@(map f args)))
     (define (sc->constraints v f)
       (match-define (struct-combinator args _ mut?) v)
       (merge-restricts* (if mut? 'chaperone 'flat)
                         (for/list ([a (in-list args)])
                           (if (not mut?)
                               (add-constraint (f a) 'chaperone)
                               (f a)))))])

(define (struct/sc name mut? fields)
  (struct-combinator fields name mut?))

(define-match-expander struct/sc:
  (syntax-parser
    [(_ name fields)
     #'(struct-combinator fields name _)]))

;; FIXME: Currently ignores the structure type and fails on all
;; reflective use.  
(struct struct-type/sc combinator ()
  #:transparent
  #:property prop:combinator-name "struct-type/sc"
  #:methods gen:sc
    [(define (sc-map v f)
       (match-define (struct-type/sc args) v)
       (struct-type/sc (map (λ (a) (f a 'covariant)) args)))
     (define (sc-traverse v f)
       (match-define (struct-type/sc args) v)
       (for-each (λ (a) (f a 'covariant)) args)
       (void))
     (define (sc->contract v f)
       (match-define (struct-type/sc args) v)
       #`(struct-type/c #f))
     (define (sc->constraints v f)
       (match-define (struct-type/sc args) v)
       (simple-contract-restrict 'chaperone))])

(define-match-expander struct-type/sc:
  (syntax-parser
    [(_ args)
     #'(struct-type/sc args)]))
