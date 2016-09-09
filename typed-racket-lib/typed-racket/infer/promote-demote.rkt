#lang racket/base

(require "../utils/utils.rkt"
         (rep type-rep values-rep rep-utils free-variance)
         (types abbrev utils)
         (prefix-in c: (contract-req))
         racket/list racket/match)
(provide/cond-contract
  [var-promote (c:-> Type? (c:listof symbol?) Type?)]
  [var-demote (c:-> Type? (c:listof symbol?) Type?)])

(define (V-in? V . ts)
  (for/or ([e (in-list (append* (map fv ts)))])
    (memq e V)))

;; get-propset : SomeValues -> PropSet
;; extract prop sets out of the range of a function type
(define (get-propsets rng)
  (match rng
    [(AnyValues: p) (list (-PS p p))]
    [(Values: (list (Result: _ propsets _) ...)) propsets]
    [(ValuesDots: (list (Result: _ propsets _) ...) _ _) propsets]))


(define (var-promote T V)
  (var-change V T #t))
(define (var-demote T V)
  (var-change V T #f))



(define (var-change V cur change)
  (define (co t) (var-change V t change))
  (define (contra t) (var-change V t (not change)))
  ;; arr? -> (or/c #f arr?)
  ;; Returns the changed arr or #f if there is no arr above it
  (define (arr-change arr)
    (match arr
      [(arr: dom rng rest drest kws)
       (cond
         [(apply V-in? V (get-propsets rng))
          #f]
         [(and drest (memq (cdr drest) V))
          (make-arr (map contra dom)
                    (co rng)
                    (contra (car drest))
                    #f
                    (map contra kws))]
         [else
          (make-arr (map contra dom)
                    (co rng)
                    (and rest (contra rest))
                    (and drest (cons (contra (car drest)) (cdr drest)))
                    (map contra kws))])]))
  (match cur
    [(? structural? t)
     (define mk (Rep-constructor t))
     (apply mk (for/list ([t (in-list (Rep-values t))]
                          [v (in-list (Type-variances t))])
                 (cond
                   [(eq? v Covariant) (co t)]
                   [(eq? v Invariant)
                    (if (V-in? V t)
                        (if change Univ -Bottom)
                        t)]
                   [(eq? v Contravariant)
                    (contra t)])))]
    [(Unit: imports exports init-depends t)
     (make-Unit (map co imports)
                (map contra imports)
                (map co init-depends)
                (co t))]
    [(F: name) (if (memq name V)
                   (if change Univ -Bottom)
                   cur)]
    [(Function: arrs)
     (make-Function (filter-map arr-change arrs))]
    [(HeterogeneousVector: elems)
     (make-HeterogeneousVector (map (λ (t) (if (V-in? V t)
                                               (if change Univ -Bottom)
                                               t))
                                    elems))]
    [_ (Rep-fold co cur)]))
