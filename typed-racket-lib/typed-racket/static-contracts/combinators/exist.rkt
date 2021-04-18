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
     (match-define (exist-combinator (list names lhs rhs)) v)
     (exist-combinator (list names (f lhs 'invariant) (f rhs 'invariant))))
   (define (sc-traverse v f)
     (match-define (exist-combinator (list _ lhs rhs)) v)
     (f lhs 'invariant)
     (f rhs 'invariant)
     (void))
   (define (sc->contract v f)
     (match v
       [(exist-combinator (list names lhs rhs))
        (parameterize ([static-contract-may-contain-free-ids? #t])
          (let ([a (with-syntax ([lhs-stx (f lhs)]
                                 [rhs-stx (f rhs)]
                                 [n (car names)])
                     #'(->i ([n lhs-stx])
                            (_ (n)
                               rhs-stx)))])
            a))]))
   (define (sc->constraints v f)
     (simple-contract-restrict 'flat))])


(define (exist/sc names lhs rhs)
  (exist-combinator (list names lhs rhs)))

(define-match-expander exist/sc:
  (syntax-parser
    [(_ names lhs rhs rhs-deps)
     #'(exist-combinator (list names lhs rhs))]))
