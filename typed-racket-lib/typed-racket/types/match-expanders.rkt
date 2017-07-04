#lang racket/base

(require "../utils/utils.rkt"
         (rep type-rep values-rep rep-utils)
         racket/match
         syntax/parse/define
         racket/set
         racket/unsafe/undefined
         (types resolve base-abbrev)
         (for-syntax racket/base syntax/parse))

(provide Listof: List: MListof: AnyPoly: AnyPoly-names:
         HashTableTop:
         SimpleListof: SimpleMListof:
         PredicateProp:
         Val-able:
         Is-a:)


;; matches types that are exactly the pattern,
;; or that are an intersection type of some kind where
;; exactly 1 of the types in the intersection is of the type
(define-match-expander Is-a:
  (lambda (stx)
    (syntax-parse stx
      [(_ pat)
       (syntax/loc stx
         (or pat
             (Intersection: (or (list pat)
                                (app (λ (ts) (filter (match-lambda
                                                       [pat #t]
                                                       [_ #f])
                                                     ts))
                                     (list pat)))
                            _)))])))

;; some types used to be represented by a Value rep,
;; but are now represented by a Base rep. This function
;; helps us recover the singleton values for those types.
(define (Base->val? b)
  (match b
    [(== -Null) (box-immutable '())]
    [(== -Void) (box-immutable (void))]
    [(== -True) (box-immutable #t)]
    [(== -False) (box-immutable #f)]
    [(== -Zero) (box-immutable 0)]
    [(== -One) (box-immutable 1)]
    [(== -Unsafe-Undefined) (box-immutable unsafe-undefined)]
    [_ #f]))

;; matches types that correspond to singleton values
(define-match-expander Val-able:
  (lambda (stx)
    (syntax-parse stx
      [(_ pat)
       (syntax/loc stx
         (Is-a: (or (Value: pat)
                    (app Base->val? (box pat)))))])))

(define-match-expander Listof:
  (lambda (stx)
    (syntax-parse stx
      [(_ elem-pat)
       (syntax/loc stx
         (app Listof? (? Type? elem-pat)))])))

(define-match-expander SimpleListof:
  (lambda (stx)
    (syntax-parse stx
      [(_ elem-pat)
       (syntax/loc stx
         (app (λ (t) (Listof? t #t)) (? Type? elem-pat)))])))


(define-simple-macro (make-Listof-pred listof-pred?:id pair-matcher:id)
  (define (listof-pred? t [simple? #f])
    (match t
      [(Mu-unsafe:
        (Union: (== -Null)
                (list (pair-matcher elem-t (B: 0)))))
       (define elem-t* (instantiate-type elem-t t))
       (cond
         [simple? (and (equal? elem-t elem-t*) elem-t)]
         [else elem-t*])]
      [(Union: (== -Null) (list (pair-matcher hd-t tl-t)))
       (cond
         [(listof-pred? tl-t)
          => (λ (lst-t) (and (equal? hd-t lst-t) hd-t))]
         [else #f])]
      [_ #f])))

(make-Listof-pred Listof? Pair:)
(make-Listof-pred MListof? MPair:)



(define-match-expander List:
  (lambda (stx)
    (syntax-parse stx
      [(_ elem-pats)
       #'(? Type? (app untuple (? values elem-pats) (== -Null)))]
      [(_ elem-pats #:tail tail-pat)
       #'(? Type? (app untuple (? values elem-pats) tail-pat))])))

(define-match-expander MListof:
  (lambda (stx)
    (syntax-parse stx
      [(_ elem-pat)
       (syntax/loc stx (app MListof? (? Type? elem-pat)))])))

(define-match-expander SimpleMListof:
  (lambda (stx)
    (syntax-parse stx
      [(_ elem-pat)
       (syntax/loc stx (app (λ (t) (MListof? t #t)) (? Type? elem-pat)))])))


;; Type? -> (or/c (values/c #f #f) (values/c (listof Type?) Type?)))
;; Returns the prefix of types that are consed on to the last type (a non finite-pair type).
;; The last type may contain pairs if it is a list type.
(define (untuple t)
  (let loop ([t t]
             [seen (set)])
    (if (not (set-member? seen t))
        (match (resolve t)
          [(Pair: a b)
           (define-values (elems tail) (loop b (set-add seen t)))
           (values (cons a elems) tail)]
          [_ (values null t)])
        (values null t))))

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


;; A match expander for matching the prop on a predicate. This assumes a standard
;; predicate type of the shape (-> Any Any : SomeType)
(define-match-expander PredicateProp:
  (λ (stx)
    (syntax-parse stx
      [(_ ps)
       #'(Fun: (list (Arrow: (list _)
                             _
                             _
                             (Values: (list (Result: _ ps _))))))])))

(define-match-expander HashTableTop:
  (lambda (stx)
    (syntax-parse stx
     [(_) #'(Union-all: (list-no-order (Immutable-HashTable: (Univ:) (Univ:))
                                       (Mutable-HashTableTop:)
                                       (Weak-HashTableTop:)))])))

