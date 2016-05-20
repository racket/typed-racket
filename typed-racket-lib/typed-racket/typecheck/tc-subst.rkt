#lang racket/base

;; Functions in this file implement the substitution function in
;; figure 8, pg 8 of "Logical Types for Untyped Languages"

(require "../utils/utils.rkt"
         racket/match racket/list
         (contract-req)
         (except-in (types abbrev utils prop-ops path-type)
                    -> ->* one-of/c)
         (only-in (infer infer) intersect)
         (rep type-rep object-rep prop-rep rep-utils))

(provide add-scope)

(provide/cond-contract
  [restrict-values (-> SomeValues/c (listof Type/c) SomeValues/c)]
  [values->tc-results (->* (SomeValues/c (listof Object?)) ((listof Type/c)) full-tc-results/c)]
  [replace-names (-> (listof (list/c identifier? Object?)) tc-results/c tc-results/c)])

;; Substitutes the given objects into the values and turns it into a tc-result.
;; This matches up to the substitutions in the T-App rule from the ICFP paper.
(define (values->tc-results v os [ts (map (λ (o) Univ) os)])
  (define res
    (match v
      [(AnyValues: f)
       (tc-any-results f)]
      [(Results: t f o)
       (ret t f o)]
      [(Results: t f o dty dbound)
       (ret t f o dty dbound)]))
  (for/fold ([res res]) ([(o arg) (in-indexed (in-list os))]
                         [t (in-list ts)])
    (subst-tc-results res (list 0 arg) o #t t)))

;; Restrict the objects in v refering to the current functions arguments to be of the types ts.
(define (restrict-values v ts)
  (for/fold ([v v]) ([t (in-list ts)] [arg (in-naturals)])
    (subst-type v (list 0 arg) (-arg-path arg) #t t)))

;; replace-names: (listof (list/c identifier? Object?) tc-results? -> tc-results?
;; For each name replaces all uses of it in res with the corresponding object.
;; This is used so that names do not escape the scope of their definitions
(define (replace-names names+objects res)
  (for/fold ([res res]) ([name/object (in-list names+objects)])
    (subst-tc-results res (first name/object) (second name/object) #t Univ)))

;; Substitution of objects into a tc-results
;; This is a combination of all of thes substitions from the paper over the different parts of the
;; results.
;; t is the type of the object that we are substituting in. This allows for restriction/simplification
;; of some props if they conflict with the argument type.
(define/cond-contract (subst-tc-results res k o polarity t)
  (-> full-tc-results/c name-ref/c Object? boolean? Type? full-tc-results/c)
  (define (st ty) (subst-type ty k o polarity t))
  (define (sr ty fs ob) (subst-tc-result ty fs ob k o polarity t))
  (define (sf f) (subst-prop f k o polarity t))
  (match res
    [(tc-any-results: f) (tc-any-results (sf f))]
    [(tc-results: ts fs os)
     (tc-results (map sr ts fs os) #f)]
    [(tc-results: ts fs os dt db)
     (tc-results (map sr ts fs os) (cons (st dt) db))]))


;; Substitution of objects into a tc-result
;; This is a combination of the other substitutions, plus a restriction of the returned type
;; to the arguments type if the returned object corresponds to an argument.
(define (subst-tc-result r-t r-fs r-o k o polarity t)
  (define argument-side
    (match r-o
      [(Path: p (? (lambda (nm) (name-ref=? nm k))))
       (path-type p t)]
      [_ Err]))

  (tc-result
    (if (equal? argument-side Err)
        (subst-type r-t k o polarity t)
        (intersect argument-side
                   (subst-type r-t k o polarity t)))
    (subst-prop-set r-fs k o polarity t)
    (subst-object r-o k o polarity)))

;; Substitution of objects into a prop set
;; This is essentially ψ+|ψ- [o/x] from the paper
(define/cond-contract (subst-prop-set pset k o polarity t)
  (-> (or/c #f PropSet?) name-ref/c Object? boolean? Type/c PropSet?)
  (define extra-prop (-is-type k t))
  (define (add-extra-prop p)
    (define p* (-and p extra-prop))
    (cond
      [(prop-equal? p* extra-prop) -tt]
      [(FalseProp? p*) -ff]
      [else p]))
  (match pset
    [(PropSet: p+ p-)
     (-PS (subst-prop (add-extra-prop p+) k o polarity t)
          (subst-prop (add-extra-prop p-) k o polarity t))]
    [_ -tt-propset]))

;; Substitution of objects into a type
;; This is essentially t [o/x] from the paper
(define/cond-contract (subst-type t k o polarity ty)
  (-> Type? name-ref/c Object? boolean? Type/c Type?)
  (define (st t) (subst-type t k o polarity ty))
  (define/cond-contract (sf fs) (PropSet? . -> . PropSet?) (subst-prop-set fs k o polarity ty))
  (type-case (#:Type st
              #:Prop sf
              #:Object (lambda (f) (subst-object f k o polarity)))
              t
              [#:arr dom rng rest drest kws
                     (let* ([st* (λ (t) (subst-type t (add-scope k) (add-scope/object o) polarity ty))])
                       (make-arr (map st dom)
                                 (st* rng)
                                 (and rest (st rest))
                                 (and drest (cons (st (car drest)) (cdr drest)))
                                 (map st kws)))]))

;; add-scope : name-ref/c -> name-ref/c
;; Add a scope to an index name-ref
(define (add-scope key)
  (match key
    [(list fun arg) (list (add1 fun) arg)]
    [(? identifier?) key]))

;; add-scope/object : Object? -> Object?
;; Add a scope to an index object
(define (add-scope/object obj)
  (match obj
    [(Empty:) -empty-obj]
    [(Path: p nm) (make-Path p (add-scope nm))]))

;; Substitution of objects into objects
;; This is o [o'/x] from the paper
(define/cond-contract (subst-object t k o polarity)
  (-> Object? name-ref/c Object? boolean? Object?)
  (match t
    [#f t]
    [(Empty:) t]
    [(Path: p i)
     (if (name-ref=? i k)
         (match o
           [(Empty:) -empty-obj]
           ;; the result is not from an annotation, so it isn't a NoObject
           [#f -empty-obj]
           [(Path: p* i*) (make-Path (append p p*) i*)])
         t)]))

;; Substitution of objects into a prop in a prop set
;; This is ψ+ [o/x] and ψ- [o/x] with the addition that props are restricted to
;; only those values which are a subtype of the actual argument type (ty).
(define/cond-contract (subst-prop p k o polarity ty)
  (-> Prop? name-ref/c Object? boolean? Type/c Prop?)
  (define (ap q) (subst-prop q k o polarity ty))
  (define (tprop-matcher pes i t maker)
    (cond
      [(name-ref=? i k)
       (match o
         [(Empty:)
          (if polarity -tt -ff)]
         [_
          ;; `ty` alone doesn't account for the path, so
          ;; first traverse it with the path to match `t`
          (define ty/path (path-type pes ty))
          (maker
           (-acc-path pes o)
           ;; don't intersect if the path doesn't match the type
           (if (equal? ty/path Err)
               (subst-type t k o polarity ty)
               (intersect ty/path
                          (subst-type t k o polarity ty))))])]
      [else p]))

  (match p
    [(AndProp: ps) (apply -and (map ap ps))]
    [(OrProp: ps) (apply -or (map ap ps))]
    [(FalseProp:) -ff]
    [(TrueProp:) -tt]
    [(TypeProp: (Path: pes i) t)
     (tprop-matcher pes i t -is-type)]
    [(NotTypeProp: (Path: pes i) t)
     (tprop-matcher pes i t -not-type)]))
