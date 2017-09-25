#lang racket/base

;; Static contract for dependent ->.

(require "../../utils/utils.rkt"
         "../structures.rkt"
         "../constraints.rkt"
         "any.rkt"
         racket/match
         racket/list
         (for-template racket/base racket/contract/base)
         (contract-req))

(define depth 0)

(struct ->i/sc static-contract (typed-side?
                                ids
                                dom
                                dom-deps
                                pre
                                pre-deps
                                rng
                                rng-deps)
  #:transparent
  #:property prop:combinator-name "dep->/sc"
  #:methods gen:sc
  [(define (sc->contract v rec)
     (match v
       [(->i/sc typed-side? ids dom/scs dom-deps pre pre-deps rng/scs rng-deps)
        (with-syntax ([(id ...) ids]
                      [(c ...) (for/list ([d/sc (in-list dom/scs)]
                                          [dep-ids (in-list dom-deps)])
                                 (cond
                                   [(not (null? dep-ids))
                                    (parameterize ([static-contract-may-contain-free-ids? #t])
                                      (rec d/sc))]
                                   [else (rec d/sc)]))]
                      [(dep ...) dom-deps]
                      [(r-deps ...) rng-deps]
                      [(p-deps ...) pre-deps])
          #`(->i ([id dep c] ...)
                 #,@(cond
                      [(not pre) #'()]
                      [else #`(#:pre (p-deps ...)
                               #,(cond
                                   [(not (null? pre-deps))
                                    (parameterize ([static-contract-may-contain-free-ids? #t])
                                      (rec pre))]
                                   [else (rec pre)]))])
                 #,(cond
                     [(and typed-side? (andmap any/sc? rng-deps)) #'any]
                     [(null? rng-deps)
                      #`[_ () (values #,@(map rec rng/scs))]]
                     [else
                      (parameterize ([static-contract-may-contain-free-ids? #t])
                        #`[_ (r-deps ...) (values #,@(map rec rng/scs))])])))]))
   (define (sc-map v f)
     (match v
       [(->i/sc typed-side? ids dom/scs dom-deps pre pre-deps rng/scs rng-deps)
        (->i/sc typed-side?
                ids
                (for/list ([d/sc (in-list dom/scs)])
                  (f d/sc 'contravariant))
                dom-deps
                (and pre (f pre 'contravariant))
                pre-deps
                (for/list ([r/sc (in-list rng/scs)])
                  (f r/sc 'covariant))
                rng-deps)]))
   (define (sc-traverse v f)
     (match v
       [(->i/sc _ _ dom/scs _ pre _ rng/scs _)
        (for ([d/sc (in-list dom/scs)])
          (f d/sc 'contravariant))
        (when pre (f pre 'contravariant))
        (for ([r/sc (in-list rng/scs)])
          (f r/sc 'covariant))]))
   (define (sc-terminal-kind v) 'impersonator)
   (define (sc->constraints v f)
     (match v
       [(->i/sc _ _ dom/scs _ pre _ rng/scs _)
        (merge-restricts* 'impersonator
                          (append (if pre (list (f pre)) (list))
                                  (map f rng/scs)
                                  (map f dom/scs)))]))])

(require-for-cond-contract "proposition.rkt")

(provide/cond-contract
 [struct ->i/sc ([typed-side? boolean?]
                          [ids (listof identifier?)]
                          [dom (listof static-contract?)]
                          [dom-deps (listof (listof identifier?))]
                          [pre (or/c #f proposition-contract?)]
                          [pre-deps (listof (listof identifier?))]
                          [rng (listof static-contract?)]
                          [rng-deps (listof identifier?)])])
