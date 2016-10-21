#lang racket/base

;; Implements a check to determine if a part of a static contract has two
;; (or more) parametric contracts as direct descendents.

(require
  "../utils/utils.rkt"
  (contract-req)
  racket/match
  syntax/private/id-table
  "structures.rkt"
  "equations.rkt"
  "combinators/parametric.rkt"
  "combinators/structural.rkt")

(provide/cond-contract
 [parametric-check (static-contract? . -> . boolean?)])


(define (parametric-check sc)

  (define eqs (make-equation-set))
  (define vars (make-hash))
  (define rec-vars (make-free-id-table))

  (define (get-var sc)
    (hash-ref! vars sc (lambda () (add-variable! eqs 0))))
  (define (get-rec-var id)
    (free-id-table-ref! rec-vars id (λ () (add-variable! eqs 0))))

  (define seen (make-hash))

  (define (recur sc variance)
    (define seen? #f)
    (match sc
      ;; skip already seen sc
      [_ #:when (hash-ref seen (list sc variance) #f)
       (set! seen? #t)]
      [(or (or/sc: elems ...) (and/sc: elems ...))
       (add-equation! eqs (get-var sc)
                      (lambda () (for/sum ([e (in-list elems)])
                                   (variable-ref (get-var e)))))]
      [(or (parametric-var/sc: id) (sealing-var/sc: id))
       (add-equation! eqs (get-var sc) (lambda () 1))]
      [(recursive-sc names values body)
       (for ([name (in-list names)]
             [value (in-list values)])
         (add-equation! eqs (get-rec-var name) (lambda () (variable-ref (get-var value)))))
       (add-equation! eqs (get-var sc) (lambda () (variable-ref (get-var body))))]
      [(recursive-sc-use id)
       (add-equation! eqs (get-var sc) (lambda () (variable-ref (get-rec-var id))))]
      [else
       (get-var sc)])
    (unless seen?
      (hash-set! seen (list sc variance) #t)
      (sc-traverse sc recur)))

  (recur sc 'covariant)

  (for/or ([(k v) (in-hash (resolve-equations eqs))]
           #:when (>= v 2))
    #t))
