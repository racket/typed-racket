#lang racket/base
(require "../../utils/utils.rkt"
         "../structures.rkt"
         "../constraints.rkt"
         racket/list
         racket/match
         racket/syntax
         (contract-req)
         (for-template racket/base racket/contract/base)
         (for-syntax racket/base syntax/parse))

(provide exist/sc:)

(provide/cond-contract
 [exist/sc ((listof identifier?) static-contract? static-contract? . -> . static-contract?)])


(struct exist-combinator combinator ()
  #:transparent
  #:methods gen:sc
  [(define (sc-map v f)
     (match-define (exist-combinator (list names doms rngs)) v)
     (exist-combinator (list names (f doms 'invariant) (f rngs 'invariant))))
   (define (sc-traverse v f)
     (match-define (exist-combinator (list _ doms rngs)) v)
     (f doms 'invariant)
     (f rngs 'invariant)
     (void))
   (define (sc->contract v f)
     (match v
       [(exist-combinator (list names doms rngs))
        (parameterize ([static-contract-may-contain-free-ids? #t])
          (let ([a (with-syntax ([doms-stx (f doms)]
                                 [rngs-stx (f rngs)]
                                 [n (car names)])
                     #'(->i ([n doms-stx])
                            (_ (n)
                               rngs-stx)))])
            a))]))
   (define (sc->constraints v f)
     (simple-contract-restrict 'flat))])


(define (exist/sc names doms rngs)
  (exist-combinator (list names doms rngs)))

(define-match-expander exist/sc:
  (syntax-parser
    [(_ names doms rngs rngs-deps)
     #'(exist-combinator (list names doms rngs))]))
