#lang racket/base

(require "../utils/utils.rkt"
         (rep type-rep rep-utils type-mask)
         (types abbrev subtype resolve utils)
         racket/match)

(provide subtract)


;; subtract
;; Type Type -> Type
;; conservatively calculates set subtraction
;; between the types (i.e. t - s)
(define (subtract t s [obj -empty-obj])
  (define s-mask (mask s))
  (define result
    (let recurse ([t t] [obj obj])
      (define (sub t [obj -empty-obj]) (recurse t obj))
      (match t
        [_ #:when (disjoint-masks? (mask t) s-mask) t]
        [_ #:when (subtype t s obj) -Bottom]
        [(or (App: _ _) (? Name?))
         ;; must be different, since they're not subtypes
         ;; and n must refer to a distinct struct type
         t]
        [(BaseUnion: bbits nbits)
         (match s
           [(Base-bits: num? bits)
            (if num?
                (make-BaseUnion bbits (nbits-subtract nbits bits))
                (make-BaseUnion (bbits-subtract bbits bits) nbits))]
           [(BaseUnion: bbits* nbits*)
            (make-BaseUnion (bbits-subtract bbits bbits*)
                            (nbits-subtract nbits nbits*))]
           [_ (apply Un (for/list ([b (in-list (BaseUnion-bases t))])
                          (sub b obj)))])]
        [(Union: base elems) (Union-fmap (λ (t) (sub t obj)) base elems)]
        [(Intersection: ts raw-prop)
         (-refine (apply -unsafe-intersect (map (λ (t) (sub t obj)) ts))
                  raw-prop)]
        [(? Mu?) (sub (unfold t) obj)]
        [(Poly: vs b) (make-Poly vs (sub b))]
        [_ t])))
  (cond
    [(subtype t result obj) t]
    [else result]))
