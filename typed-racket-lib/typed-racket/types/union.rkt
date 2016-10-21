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

;; a is a Type (not a union type)
;; b is a List[Type] (non overlapping, non Union-types)
;; The output is a non overlapping list of non Union types.
;; The overlapping constraint is lifted if we are in the midst of subtyping. This is because during
;; subtyping calls to subtype are expensive.
(define (merge a b)
  (let ([a (normalize-type a)])
    (define b* (make-Union (list->hset b)))
    (cond
      [(subtype b* a) (list a)]
      [(subtype a b*) b]
      [else (cons a (filter-not (λ (b-elem) (subtype b-elem a)) b))])))

;; Type -> List[Type]
(define (flat t)
  (match t
    [(Union: es) (hset->list es)]
    [_ (list t)]))

;; Recursively reduce unions so that they do not contain
;; reduntant information w.r.t. subtyping. We used to maintain
;; this properly throughout typechecking, but this was costly.
;; Not we only do it as we are generating contracts, since we
;; don't want to do redundant runtime checks, etc.
(define (normalize-type t)
  (match t
    [(Union: ts) (make-Union (list->hset (foldr merge '() (append-map flat (hset->list ts)))))]
    [_ (Rep-fmap t normalize-type)]))
