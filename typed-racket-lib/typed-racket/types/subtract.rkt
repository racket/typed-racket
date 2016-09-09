#lang racket/base

(require "../utils/utils.rkt"
         (rep type-rep rep-utils type-mask)
         (types abbrev union subtype resolve utils)
         racket/match racket/set)

(provide subtract)


;; subtract
;; Type Type -> Type
;; conservatively calculates set subtraction
;; between the types (i.e. t - s)
(define (subtract t s)
  (define result
    (let sub ([t t])
      (match t
        [_ #:when (disjoint-masks? (Type-mask t) (Type-mask s)) t]
        [_ #:when (subtype t s) -Bottom]
        [(or (App: _ _ _) (? Name?))
         ;; must be different, since they're not subtypes
         ;; and n must refer to a distinct struct type
         t]
        [(Union: elems) (apply Un (map sub elems))]
        [(Intersection: ts)
         (apply -unsafe-intersect (set-map ts sub))]
        [(? Mu?) (sub (unfold t))]
        [(Poly: vs b) (make-Poly vs (sub b))]
        [_ t])))
  (cond
    [(subtype t result) t]
    [else result]))
