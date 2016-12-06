#lang racket/base

(require "../utils/utils.rkt"
         (utils hset)
         (rep type-rep rep-utils)
         (prefix-in c: (contract-req))
         (types subtype base-abbrev resolve current-seen)
         racket/match
         racket/list)

(provide normalize-type
         Un
         union)

;; t1 ∪ t2
;; But excludes duplicate info w.r.t. subtyping
;; can be useful in a few places, but avoid using
;; in hot code when Un (or similar) will suffice.
(define (union t1 t2)
  (cond
    [(subtype t1 t2) t2]
    [(subtype t2 t1) t1]
    [else (Un t1 t2)]))

;; t is a Type (not a union type)
;; b is a hset[Type] (non overlapping, non Union-types)
;; The output is a non overlapping hset of non Union types.
(define (merge t ts)
  (let ([t (normalize-type t)])
    (define t* (make-Union ts))
    (cond
      [(subtype t* t) (hset t)]
      [(subtype t t*) ts]
      [else (hset-add (hset-filter ts (λ (b-elem) (not (subtype b-elem t))))
                      t)])))

;; list[Type] -> hset[Type]
(define (flatten ts)
  (for/fold ([s (hset)])
            ([t (in-hset ts)])
    (match t
      [(Union: ts) (hset-union s ts)]
      [_ (hset-add s t)])))

;; Recursively reduce unions so that they do not contain
;; reduntant information w.r.t. subtyping. We used to maintain
;; this properly throughout typechecking, but this was costly.
;; Not we only do it as we are generating contracts, since we
;; don't want to do redundant runtime checks, etc.
(define (normalize-type t)
  (match t
    [(Union: ts) (make-Union (for/fold ([ts (hset)])
                                       ([t (in-hset (flatten ts))])
                               (merge t ts)))]
    [_ (Rep-fmap t normalize-type)]))
