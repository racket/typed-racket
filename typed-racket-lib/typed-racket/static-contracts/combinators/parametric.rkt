#lang racket/base

;; Static contract for parametric->/c and sealing->/sc.

(require
  "../structures.rkt"
  "../constraints.rkt"
  "../terminal.rkt"
  racket/match
  racket/contract
  (for-template racket/base racket/contract/parametric
                typed-racket/utils/sealing-contract)
  (for-syntax racket/base syntax/parse))

(provide
  (contract-out
    [parametric->/sc ((listof identifier?) static-contract? . -> . static-contract?)]
    [parametric-var/sc (identifier? . -> . static-contract?)]
    [sealing->/sc ((listof identifier?)
                   (list/c (listof symbol?) (listof symbol?) (listof symbol?))
                   static-contract? . -> . static-contract?)]
    [sealing-var/sc (identifier? . -> . static-contract?)])
  parametric->/sc:
  sealing->/sc:
  (rename-out
    [parametric-var/sc parametric-var/sc:]
    [parametric-combinator? parametric->/sc?]
    [sealing-var/sc sealing-var/sc:]
    [sealing-combinator? sealing->/sc?]))


(struct parametric-combinator combinator (vars)
  #:transparent
  #:property prop:combinator-name "parametric->/sc"
  #:methods gen:sc
    [(define (sc-map v f)
       (match v
        [(parametric-combinator (list arg) vars)
         (parametric-combinator (list (f arg 'covariant)) vars)]))
     (define (sc-traverse v f)
       (match v
        [(parametric-combinator (list arg) vars)
         (f arg 'covariant)
         (void)]))
     (define (sc->contract v f)
       (match v
        [(parametric-combinator (list arg) vars)
         #`(parametric->/c #,vars #,(f arg))]))
     (define (sc->constraints v f)
       (match v
        [(parametric-combinator (list arg) vars)
         (merge-restricts* 'impersonator  (list (f arg)))]))])

(define (parametric->/sc vars body)
  (parametric-combinator (list body) vars))

(define-match-expander parametric->/sc:
  (syntax-parser
    [(_ vars body)
     #'(parametric-combinator (list body) vars)]))

(define-terminal-sc parametric-var/sc (id) #:impersonator
  #:printer (v p mode) (display (syntax-e (parametric-var/sc-id v)) p)
   id)

;; combinator for sealing-> contracts for row polymorphism
(struct sealing-combinator combinator (vars members)
  #:transparent
  #:property prop:combinator-name "sealing->/sc"
  #:methods gen:sc
    [(define (sc-map v f)
       (match v
        [(sealing-combinator (list arg) vars members)
         (sealing-combinator (list (f arg 'covariant)) vars members)]))
     (define (sc-traverse v f)
       (match v
        [(sealing-combinator (list arg) vars members)
         (f arg 'covariant)
         (void)]))
     (define (sc->contract v f)
       (match v
        [(sealing-combinator (list arg) vars members)
         #`(sealing->/c #,(car vars) #,members #,(f arg))]))
     (define (sc->constraints v f)
       (match v
        [(sealing-combinator (list arg) vars members)
         (merge-restricts* 'impersonator  (list (f arg)))]))])

(define (sealing->/sc vars members body)
  (sealing-combinator (list body) vars members))

(define-match-expander sealing->/sc:
  (syntax-parser
    [(_ vars members body)
     #'(sealing-combinator (list body) vars members)]))

(define-terminal-sc sealing-var/sc (id) #:impersonator
  #:printer (v p mode) (display (syntax-e (sealing-var/sc-id v)) p)
   id)
