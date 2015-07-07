#lang racket

;; This module provides a function pairwise-intersect for calculating a lower
;; bound of two types with respect to the precision order (*not* the subtype
;; order). Contract typechecking uses this function to determine the type of a
;; contracted value, given the value's type and the contract's output type.

(require "../utils/utils.rkt"
         (rep type-rep prop-rep object-rep values-rep)
         (types subtype)
         (only-in (infer infer) intersect)
         racket/match)

(provide pairwise-intersect)

(define (pairwise-intersect/arr s t)
  (match* (s t)
    [((Arrow: s-dom s-rest s-kws s-rng)
      (Arrow: t-dom t-rest t-kws t-rng))
     #:when (= (length s-kws) (length t-kws))
     (make-Arrow (map pairwise-intersect s-dom t-dom)
                 (and s-rest t-rest
                      (pairwise-intersect s-rest t-rest))
                 (map pairwise-intersect/kw s-kws t-kws)
                 (pairwise-intersect s-rng t-rng))]
    [(_ _)
     (raise-arguments-error
      'pairwise-intersect/arr
      "pairwise-interescting unsupported for drest/different length kws"
      "s" s
      "t" t)]))

(define (pairwise-intersect/kw s t)
  (match* (s t)
    [((Keyword: ks ts rs) (Keyword: kt tt rt))
     #:when (and (eq? kt ks)
                 (or rt (not rs)))
     (make-Keyword ks (pairwise-intersect ts tt) rs)]
    [(_ _)
     (raise-arguments-error
      'pairwise-intersect/kw
      "keywords must match"
      "s" s
      "t" t)]))

(define (pairwise-intersect/prop-set ps qs)
  (match* (ps qs)
    [((PropSet: p+ p-) (PropSet: q+ q-))
     (make-PropSet (pairwise-intersect/prop p+ q+)
                   (pairwise-intersect/prop p- q-))]))

(define (pairwise-intersect/prop p1 p2)
  (match* (p1 p2)
    [(p p) p]
    [(p (TrueProp:)) p]
    [((TrueProp:) p) p]
    [((TypeProp: o s) (TypeProp: o t))
     (make-TypeProp o (pairwise-intersect s t))]
    [((NotTypeProp: o s) (NotTypeProp: o t))
     (make-NotTypeProp o (pairwise-intersect s t))]
    [(_ _)
     (raise-arguments-error
      'pairwise-intersect/prop
      "cannot merge props"
      "p1" p1
      "p2" p2)]))

;; pairwise-intersect : (case-> (-> Type Type Type)
;;                              (-> Values Values Values)
;;                              (-> Result Result Result))
;; Computes a lower bound of the two types w.r.t. the "precision order." The
;; precision order is like the subtype order except that it does not account for
;; variance. Effectively, this amounts to a fold over the two types in a uniform
;; way. For base types (and, as a default, for unimplemented cases) this
;; computes the intersection of the two types.

;; For example,
;;    (pairwise-intersect (-> Any Any) (-> Real Real)) = (-> Real Real)
;;    (pairwise-intersect (-> Nat Any) (-> Pos-Real Any)) = (-> Pos-Int Any)
;;    (pairwise-intersect (-> String Any) (-> Int Any)) = (-> Nothing Any)
(define (pairwise-intersect s t)
  (match* (s t)
    [((Univ:) u) u]
    [(u (Univ:)) u]
    [((Fun: arr1s) (Fun: arr2s))
     #:when (= (length arr1s) (length arr2s))
     (make-Fun (map pairwise-intersect/arr arr1s arr2s))]
    [((Result: ss pset-s o1) (Result: ts pset-t o2))
     (make-Result (pairwise-intersect ss ts)
                  (pairwise-intersect/prop-set pset-s pset-t)
                  (match* (o1 o2)
                    [((Empty:) o2) o2]
                    [(o1 (Empty:)) o1]
                    [(o o) o]
                    [(_ _)
                     (raise-arguments-error
                      'pairwise-intersect
                      "objects must both be identical or one must be empty"
                      "o1" o1
                      "o2" o2)]))]
    [((Values: rs) (Values: rt))
     (make-Values (map pairwise-intersect rs rt))]
    [((ValuesDots: s-rs s-dty dbound) (ValuesDots: t-rs t-dty dbound))
     (make-ValuesDots (map pairwise-intersect s-rs t-rs)
                      (pairwise-intersect s-dty t-dty)
                      dbound)]
    [((Poly: vs b) _)
     #:when (or (subtype s t) (subtype t s))
     s]
    [(_ (Poly: vs b))
     #:when (or (subtype t s) (subtype s t))
     t]
    [(_ _)
     #:when (and (Type? s) (Type? t))
     (intersect s t)]
    [(_ _)
     (raise-arguments-error
      'pairwise-intersect
      "unable to intersect"
      "s" s
      "t" t)]))
