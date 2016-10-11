#lang racket/base


(require "../utils/utils.rkt")

(require (rep type-rep values-rep rep-utils)
         racket/match
         (types resolve)
         (contract-req)
         racket/set
         (for-syntax racket/base syntax/parse))

(provide Listof: List: MListof: AnyPoly: AnyPoly-names: Function/arrs:
         PredicateProp:)


(define-match-expander Listof:
  (lambda (stx)
    (syntax-parse stx
      [(_ elem-pat (~optional var-pat #:defaults ([var-pat #'var])))
       (syntax/loc stx
         (or (Mu: var-pat (Union: (list (Value: '()) (Pair: elem-pat (F: var-pat)))))
             (Mu: var-pat (Union: (list (Pair: elem-pat (F: var-pat)) (Value: '()))))))])))

(define-match-expander List:
  (lambda (stx)
    (syntax-parse stx
      [(_ elem-pats)
       #'(? Type? (app untuple (? values elem-pats) (Value: '())))]
      [(_ elem-pats #:tail tail-pat)
       #'(? Type? (app untuple (? values elem-pats) tail-pat))])))

;; Type? -> (or/c (values/c #f #f) (values/c (listof Type?) Type?)))
;; Returns the prefix of types that are consed on to the last type (a non finite-pair type).
;; The last type may contain pairs if it is a list type.
(define (untuple t)
  (let loop ((t t) (seen (set)))
    (if (not (set-member? seen (Rep-seq t)))
        (match (resolve t)
          [(Pair: a b)
           (define-values (elems tail) (loop b (set-add seen (Rep-seq t))))
           (values (cons a elems) tail)]
          [_ (values null t)])
        (values null t))))

(define-match-expander MListof:
  (lambda (stx)
    (syntax-parse stx
      [(_ elem-pat (~optional var-pat #:defaults ([var-pat #'var])))
       ;; see note above
       #'(or (Mu: var-pat (Union: (list (Value: '()) (MPair: elem-pat (F: var-pat)))))
             (Mu: var-pat (Union: (list (MPair: elem-pat (F: var-pat)) (Value: '())))))])))

(define (unpoly t)
  (match t
    [(Poly: fixed-vars t)
     (let-values ([(vars dotted t) (unpoly t)])
       (values (append fixed-vars vars) dotted t))]
    [(PolyDots: (list fixed-vars ... dotted-var) t)
     (let-values ([(vars dotted t) (unpoly t)])
       (values (append fixed-vars vars) (cons dotted-var dotted) t))]
    [t (values null null t)]))

(define (unpoly-names t)
  (match t
    [(Poly-names: fixed-vars t)
     (let-values ([(vars dotted t) (unpoly t)])
       (values (append fixed-vars vars) dotted t))]
    [(PolyDots-names: (list fixed-vars ... dotted-var) t)
     (let-values ([(vars dotted t) (unpoly t)])
       (values (append fixed-vars vars) (cons dotted-var dotted) t))]
    [t (values null null t)]))


;; Match expanders that match any type and separate the outer layers of the poly and poly-dots, from
;; the inner non polymorphic type.
(define-match-expander AnyPoly:
  (lambda (stx)
    (syntax-parse stx
      [(_ vars dotted-vars body)
       #'(app unpoly vars dotted-vars body)])))

(define-match-expander AnyPoly-names:
  (lambda (stx)
    (syntax-parse stx
      [(_ vars dotted-vars body)
       #'(app unpoly-names vars dotted-vars body)])))

(define-match-expander Function/arrs:
  (lambda (stx)
    (syntax-parse stx
      [(_ doms rngs rests drests kws (~optional (~seq #:arrs arrs) #:defaults ([arrs #'_])))
       #'(Function: (and arrs (list (arr: doms rngs rests drests kws) (... ...))))])))

;; A match expander for matching the prop on a predicate. This assumes a standard
;; predicate type of the shape (-> Any Any : SomeType)
(define-match-expander PredicateProp:
  (λ (stx)
    (syntax-parse stx
      [(_ ps)
       #'(Function: (list (arr: (list _) (Values: (list (Result: _ ps _))) _ _ _)))])))
