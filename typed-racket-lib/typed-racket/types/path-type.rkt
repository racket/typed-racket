#lang racket/base

(require "../utils/utils.rkt"
         racket/match racket/set
         (contract-req)
         (rep object-rep type-rep values-rep)
         (utils tc-utils prefab)
         (typecheck renamer)
         (types subtype resolve numeric-tower)
         (except-in (types utils abbrev kw-types) -> ->* one-of/c))

(require-for-cond-contract (rep rep-utils))

(provide/cond-contract
 [path-type ((listof PathElem?) Type? . -> . (or/c Type? #f))])


;; returns the result of following a path into a type
;; (Listof PathElem) Type -> Type
;; Ex. '(CarPE) (Pair α β) -> α
;; resolved is the set of resolved types so far at a particular
;; path - it ensures we are making progress, that we do not
;; continue unfolding types infinitely while not progressing.
;; It is intentionally reset each time we decrease the
;; paths size on a recursive call, and maintained/extended
;; when the path does not decrease on a recursive call.
(define (path-type path t)
  (let path-type ([path (reverse path)]
                  [t t]
                  [resolved (hash)])
    (match* (t path)
      ;; empty path
      [(t (list)) t]

      ;; -- non-empty path beyond here --

      ;; pair ops
      [((Pair: t s) (cons (CarPE:) rst))
       (path-type rst t (hash))]
      [((Pair: t s) (cons (CdrPE:) rst))
       (path-type rst s (hash))]

      ;; syntax ops
      [((Syntax: t) (cons (SyntaxPE:) rst))
       (path-type rst t (hash))]

      ;; promise op
      [((Promise: t) (cons (ForcePE:) rst))
       (path-type rst t (hash))]

      ;; struct ops (non-prefab)
      [((Struct: _ _ flds _ _ _ _) (cons (StructPE: struct-ty idx) rst))
       #:when  (subtype t struct-ty)
       (match-let ([(fld: ft _ _) (list-ref flds idx)])
         (path-type rst ft (hash)))]

      ;; prefab ops
      [((Prefab: key flds) (cons (PrefabPE: path-key idx) rst))
       #:when  (prefab-key-subtype? key path-key)
       (path-type rst (list-ref flds idx) (hash))]

      ;; vector length
      [(vec-t (list (VecLenPE:)))
       #:when (subtype vec-t -VectorTop)
       -Index]

      [((Intersection: ts _) _)
       (apply -unsafe-intersect (for*/list ([t (in-list ts)]
                                            [t (in-value (path-type path t resolved))]
                                            #:when t)
                                  t))]
      [((Union: _ ts) _)
       ;; drop base types, since they are incompatible w/ a path element
       (Union-fmap (λ (t) (or (path-type path t resolved) -Bottom)) -Bottom ts)]

      ;; paths into polymorphic types
      ;; TODO can this expose unbound type indices... probably. It should be
      ;; shielded with a check for type indexes/variables/whatever.
      [((Poly: _ body-t) _) (path-type path body-t resolved)]
      [((PolyDots: _ body-t) _) (path-type path body-t resolved)]
      [((PolyRow: _ _ body-t) _) (path-type path body-t resolved)]
      [((Distinction: _ _ t) _) (path-type path t resolved)]

      ;; for private fields in classes
      [((Fun: (list (Arrow: doms _ _ (Values: (list (Result: rng _ _))))))
        (cons (FieldPE:) rst))
       (path-type rst rng (hash))]

      ;; types which need resolving
      [((? resolvable?) _) #:when (not (hash-ref resolved t #f))
       (path-type path (resolve-once t) (hash-set resolved t #t))]

      ;; type/path mismatch =(
      [(_ _) #f])))
