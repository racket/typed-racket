#lang racket/base

;; This file contains functions that work with free TERM identifiers,
;; as in the kind that can be introduced by dependent functions (DFun) or
;; 'Refine' types and are found in Objects. These functions do not reason
;; about the variables that are used for TYPE variables (i.e. F and B types).

(require "../utils/utils.rkt"
         (contract-req)
         "rep-utils.rkt"
         "object-rep.rkt"
         racket/match
         racket/set
         racket/list)

(provide/cond-contract
 [free-ids (-> Rep? (listof identifier?))]
 [arg-deps->idx-order (-> (listof (listof identifier?))
                          (listof exact-nonnegative-integer?))]
 [cycle-in-arg-deps? (-> (listof (listof identifier?))
                         (or/c #f (listof identifier?)))])



;; since we use locally nameless representation for binders,
;; any ids we see are by definition free, so we can just cruise
;; over the type AST grabbing any identifiers we see!
(define (free-ids rep)
  (define ids '())
  (let loop! ([rep rep])
    (match rep
      [(Path: flds (? identifier? id))
       (for-each loop! flds)
       (unless (member id ids free-identifier=?)
         (set! ids (cons id ids)))]
      [_ (Rep-for-each rep loop!)]))
  ids)


;; - - - - - - - - - - - - - - - - - - - - - - - - - -
;; helper function for dependent arg id calculations
;; - - - - - - - - - - - - - - - - - - - - - - - - - -


;; take an association list of identifier dependencies and
;; returns an order those identifiers may be type checked in
;; in so an id is only visited after its dependent ids are
;; visited (cycles should have _already_ been rejected!)
(define (arg-deps->idx-order deps)
  (define order '())
  (define visited '())
  (define (visit! x)
    (unless (member x visited free-identifier=?)
      (set! visited (cons x visited))
      (for ([neighbor (in-list (cdr (assoc x deps free-identifier=?)))])
        (visit! neighbor))
      (define idx (index-where deps (λ (entry) (free-identifier=? x (car entry)))))
      (set! order (cons idx order))))
  (for ([entry (in-list deps)])
    (visit! (car entry)))
  (reverse order))

;; take an association list of identifier dependencies
;; and returns a cycle (list of identifiers)
;; or #f if no cycle exists
(define (cycle-in-arg-deps? deps)
  (define visited '())
  (define (visit x seen)
    (cond
      [(member x seen free-identifier=?) (cons x seen)]
      [else
       (begin0
         (let ([seen+x (cons x seen)])
           (for/or ([neighbor (in-list (cdr (assoc x deps free-identifier=?)))])
             (and (not (member neighbor visited free-identifier=?))
                  (visit neighbor seen+x))))
         (set! visited (cons x visited)))]))
  (match (for/or ([entry (in-list deps)])
           (visit (car entry) '()))
    [#f #f]
    [(cons x xs)
     ;; if we find a path that contained a loop,
     ;; drop the portion that was not a
     ;; part of the cycle (if it exists)
     (cons x (reverse (cons x (takef xs (λ (id) (not (free-identifier=? id x)))))))]))
