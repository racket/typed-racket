#lang racket/base

(require "../utils/utils.rkt"
         (utils hset)
         (rep type-rep rep-utils type-mask)
         (types abbrev subtype resolve utils)
         racket/match)

(provide subtract)


;; subtract
;; Type Type -> Type
;; conservatively calculates set subtraction
;; between the types (i.e. t - s)
(define (subtract t s)
  (define s-mask (mask s))
  (define result
    (let sub ([t t])
      (match t
        [_ #:when (disjoint-masks? (mask t) s-mask) t]
        [_ #:when (subtype t s) -Bottom]
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
           [_ (make-Union (for/hset ([b (in-list (BaseUnion-bases t))])
                                    (sub b)))])]
        [(Union: base elems) (Union-fmap sub base elems)]
        [(Intersection: ts)
         (apply -unsafe-intersect (hset-map ts sub))]
        [(? Mu?) (sub (unfold t))]
        [(Poly: vs b) (make-Poly vs (sub b))]
        [_ t])))
  (cond
    [(subtype t result) t]
    [else result]))
