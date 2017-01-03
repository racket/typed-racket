#lang racket/base

(require "../utils/utils.rkt"
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
    (define t* (apply Un ts))
    (cond
      [(subtype t* t) (list t)]
      [(subtype t t*) ts]
      [else (cons t (filter-not (λ (ts-elem) (subtype ts-elem t)) ts))])))

;; Recursively reduce unions so that they do not contain
;; reduntant information w.r.t. subtyping. We used to maintain
;; this properly throughout typechecking, but this was costly.
;; Not we only do it as we are generating contracts, since we
;; don't want to do redundant runtime checks, etc.
(define (normalize-type t)
  (match t
    [(? BaseUnion?) t]
    [(Union-all-flat: ts) (apply Un (foldl merge '() ts))]
    [_ (Rep-fmap t normalize-type)]))
