#lang racket/base

;; Implements a check that to determine if a part of a static contract has two (or more) parametric
;; contracts as direct descendents.

(require
  "../utils/utils.rkt"
  (utils fid-hash)
  (contract-req)
  racket/match
  "structures.rkt"
  "equations.rkt"
  "combinators/parametric.rkt"
  "combinators/structural.rkt")

(provide/cond-contract
 [parametric-check (static-contract? . -> . boolean?)])


(define (parametric-check sc)

  (define eqs (make-equation-set))
  (define vars (make-hash))
  (define rec-vars (make-fid-hash))

  (define (get-var sc)
    (hash-ref! vars sc (lambda () (add-variable! eqs 0))))
  (define (get-rec-var id)
    (fid-hash-set! rec-vars id (lambda () (add-variable! eqs 0))))

  (define seen (make-hash))

  (define (recur sc variance)
    (define seen? #f)
    (match sc
      ;; skip already seen sc
      [(? (λ (sc) (hash-ref seen (cons sc variance) #f)))
       (set! seen? #t)]
      [(or (or/sc: elems ...) (and/sc: elems ...))
       (add-equation! eqs (get-var sc)
                      (lambda () (for/sum ((e elems))
                                   (variable-ref (get-var e)))))]
      [(or (parametric-var/sc: id) (sealing-var/sc: id))
       (add-equation! eqs (get-var sc) (lambda () 1))]
      [(recursive-sc names values body)
       (for ([name names] [value values])
         (add-equation! eqs (get-rec-var name) (lambda () (variable-ref (get-var value)))))
       (add-equation! eqs (get-var sc) (lambda () (variable-ref (get-var body))))]
      [(recursive-sc-use id)
       (add-equation! eqs (get-var sc) (lambda () (variable-ref (get-rec-var id))))]
      [else
       (get-var sc)])
    (unless seen?
      (hash-set! seen (cons sc variance) #t)
      (sc-traverse sc recur)))

  (recur sc 'covariant)

  (for/or ([(k v) (in-hash (resolve-equations eqs))]
           #:when (>= v 2))
    #t))
