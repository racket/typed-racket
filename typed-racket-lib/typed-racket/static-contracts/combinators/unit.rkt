#lang racket/base

;; Static contracts for unit contracts

(require "../structures.rkt" "../constraints.rkt"
         racket/list racket/match
         racket/dict
         racket/contract
         racket/syntax
         (for-template racket/base racket/unit)
         (for-syntax racket/base syntax/parse))


(provide
 (contract-out 
  [struct signature-spec ([name identifier?]
                          [members (listof identifier?)]
                          [scs (listof static-contract?)])]
  [unit/sc (-> (listof signature-spec?) 
               (listof signature-spec?)
               (listof identifier?)
               (listof static-contract?) 
               static-contract?)]))


(struct signature-spec (name members scs) #:transparent)

(struct unit-combinator combinator ()
        #:transparent
        #:property prop:combinator-name "unit/sc"
        #:methods gen:sc
        [(define (sc-map v f) 
           (match v
             [(unit-combinator unit-spec)
              (unit-combinator (unit-spec-sc-map f unit-spec))]))
         (define (sc-traverse v f)
           (match v
             [(unit-combinator unit-spec)
              (unit-spec-sc-map f unit-spec)
              (void)]))
         (define (sc->contract v f)
           (unit/sc->contract v f))
         (define (sc->constraints v f)
           (merge-restricts* 'chaperone (map f (unit-spec->list (combinator-args v)))))])

(define unit-spec->list
  (match-lambda
   [(unit-spec imports exports init-depends invoke)
    (flatten (append (filter-map signature-spec-scs imports)
                     (filter-map signature-spec-scs exports)
                     ;; init-depends do not show up because
                     ;; there are no contracts attached
                     (filter-map (lambda (x) x) invoke)))]))

(struct unit-spec (imports exports init-depends invoke) 
        #:transparent
        #:property prop:sequence unit-spec->list)

(define (unit-spec-sc-map f seq)
  (match seq
    [(unit-spec imports exports init-depends invokes)
     (unit-spec
      (map (signature-spec-sc-map f) imports)
      (map (signature-spec-sc-map f) exports)
      ;; leave init-depends alone since they don't contain contracts
      init-depends
      (map (lambda (invoke) (and invoke (f invoke 'covariant))) invokes))]))

(define ((signature-spec-sc-map f) seq)
  (match seq
    [(signature-spec name (list ids ...) (list scs ...))
     (signature-spec
      name
      ids
      (map (lambda (sc) (and sc (f sc 'invariant))) scs))]))


(define (unit/sc->contract v f)
  (match v
    [(unit-combinator 
      (unit-spec (list imports ...)
                 (list exports ...)
                 (list deps ...)
                 (list invoke/scs ...)))
     
     (define (sig-spec->syntax sig-spec)
       (match sig-spec
         [(signature-spec name members scs)
          (define member-stx
            (map (lambda (id sc) #`(#,id #,(f sc))) members scs))
          #`(#,name #,@member-stx)]))
     
     (define (invokes->contract lst)
       (cond
        ;; just a single contract
        [(= 1 (length lst))
         #`#,(f (first lst))]
        ;; values contract
        [else 
         #`(values #,@(map f lst))]))
     
     #`(unit/c
        (import #,@(map sig-spec->syntax imports))
        (export #,@(map sig-spec->syntax exports))
        (init-depend #,@deps)
        #,(invokes->contract invoke/scs))]))

(define (unit/sc imports exports init-depends invoke)
  (unit-combinator (unit-spec imports exports init-depends invoke)))
