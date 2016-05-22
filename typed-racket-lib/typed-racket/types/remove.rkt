#lang racket/base

(require "../utils/utils.rkt"
         (rep type-rep rep-utils)
         (types abbrev union subtype resolve utils)
         racket/match racket/set)

(provide remove)


;; remove
;; Type Type -> Type
;; conservatively calculates set subtraction
;; between the types (i.e. t - s)
(define (remove t s)
  (define result
    (let rem ([t t])
      (match t
        [_ #:when (subtype t s) -Bottom]
        [(or (App: _ _ _) (? Name?))
         ;; must be different, since they're not subtypes
         ;; and n must refer to a distinct struct type
         t]
        [(Union: elems) (apply Un (map rem elems))]
        [(Intersection: ts)
         (apply -unsafe-intersect (set-map ts rem))]
        [(? Mu?) (rem (unfold t))]
        [(Poly: vs b) (make-Poly vs (rem b))]
        [_ t])))
  (cond
    [(subtype t result) t]
    [else result]))
