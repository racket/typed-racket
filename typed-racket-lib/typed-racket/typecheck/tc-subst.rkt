#lang racket/base

;; Functions in this file implement the substitution function in
;; figure 8, pg 8 of "Logical Types for Untyped Languages"

(require "../utils/utils.rkt"
         racket/match racket/list
         (contract-req)
         (except-in (types abbrev utils filter-ops) -> ->* one-of/c)
         (rep type-rep object-rep filter-rep rep-utils object-ops)
         (utils tc-utils))

(provide add-scope subst-type subst-filter subst-object subst-result)

(provide/cond-contract
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


(define (subst-result res x o polarity o-ty)
  (match res
    [(AnyValues: f)
     (make-AnyValues (subst-filter f x o polarity))]
    [(or (Results: ts fsets objs)) 
     (make-Values
      (map -result
           (for/list ([t (in-list ts)])
             (subst-type t x o polarity))
           (for/list ([fset (in-list fsets)])
             (subst-filter-set fset x o polarity o-ty))
           (for/list ([obj (in-list objs)])
             (subst-object obj x o polarity))))]
    [(Results: ts fsets objs dty dbound)
     (make-ValuesDots
      (make-Values
       (map -result
            (for/list ([t (in-list ts)])
              (subst-type t x o polarity))
            (for/list ([fset (in-list fsets)])
              (subst-filter-set fset x o polarity o-ty))
            (for/list ([obj (in-list objs)])
              (subst-object obj x o polarity))))
      (subst-type dty x o polarity)
      dbound)]
    [_ (int-err "non-result given to subst-result! ~a" res)]))


;; replace-names: (listof (list/c identifier? Object?) tc-results? -> tc-results?
;; For each name replaces all uses of it in res with the corresponding object.
;; This is used so that names do not escape the scope of their definitions
(define (replace-names names+objects res)
  (for/fold ([res res]) ([name/object (in-list names+objects)])
    (subst-tc-results res (first name/object) (second name/object) #t Univ)))

;; Substitution of objects into a tc-results
;; This is a combination of all of thes substitions from the paper over the different parts of the
;; results.
;; o-ty is the type of the object that we are substituting in. This allows for simplification of some
;; filters if they conflict with the argument type.
(define/cond-contract (subst-tc-results res k o polarity o-ty)
  (-> full-tc-results/c name-ref/c Object? boolean? Type? full-tc-results/c)
  (define (st t) (subst-type t k o polarity))
  (define (sf f) (subst-filter f k o polarity))
  (define (sfs fs) (subst-filter-set fs k o polarity o-ty))
  (define (so ob) (subst-object ob k o polarity))
  (match res
    [(tc-any-results: f) (tc-any-results (sf f))]
    [(tc-results: ts fs os)
     (ret (map st ts) (map sfs fs) (map so os))]
    [(tc-results: ts fs os dt db)
     (ret (map st ts) (map sfs fs) (map so os) (st dt) db)]))


;; Substitution of objects into a filter set
;; This is essentially ψ+|ψ- [o/x] from the paper
(define/cond-contract (subst-filter-set fs k o polarity [o-ty Univ])
  (->* ((or/c FilterSet? NoFilter?) name-ref/c Object? boolean?) (Type/c) FilterSet?)
  (define extra-filter (-filter o-ty k))
  (define (add-extra-filter f)
    (define f* (-and f extra-filter))
    (cond
      [(filter-equal? f* extra-filter) -top]
      [(Bot? f*) -bot]
      [else f]))
  (match fs
    [(FilterSet: f+ f-)
     (-FS (subst-filter (add-extra-filter f+) k o polarity)
          (subst-filter (add-extra-filter f-) k o polarity))]
    [_ -top-filter]))

;; Substitution of objects into a type
;; This is essentially t [o/x] from the paper
(define/cond-contract (subst-type t k o polarity)
  (-> Type/c name-ref/c Object? boolean? Type/c)
  (define (st t) (subst-type t k o polarity))
  (define/cond-contract (sf fs) ((or/c Filter/c FilterSet?) . -> . (or/c Filter/c FilterSet?)) 
    (if (FilterSet? fs) 
        (subst-filter-set fs k o polarity)
        (subst-filter fs k o polarity))) ;this seems wrong???
  ;(define/cond-contract (sf f) (Filter/c . -> . Filter/c) (subst-filter f k o polarity))
  (type-case (#:Type st #:Filter sf #:Object (λ (f) (subst-object f k o polarity)))
    t
    [#:arr dom rng rest drest kws dep?
           (let ([st* (λ (t) (subst-type t (add-scope k) (add-scope/object o) polarity))])
             (make-arr (map st dom)
                       (st* rng)
                       (and rest (st rest))
                       (and drest (cons (st (car drest)) (cdr drest)))
                       (map st kws)
                       dep?))]
    [#:InstdRef type prop
           (let ([st* (λ (t) (subst-type t (add-scope k) (add-scope/object o) polarity))]
                 [sf* (λ (f) (subst-filter f (add-scope k) (add-scope/object o) polarity))])
             (make-InstdRef (st* type)
                            (sf* prop)))]))

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
    [(Path: p nm) (make-Path p (add-scope nm))]
    [(? LExp? l) (LExp-path-map add-scope/object l)]))

;; Substitution of objects into objects
;; This is o [o'/x] from the paper
(define/cond-contract (subst-object t k o polarity)
  (-> Object? name-ref/c Object? boolean? Object?)
  (define (sub-o o*) (subst-object o* k o polarity))
  (match t
    [(NoObject:) t]
    [(Empty:) t]
    [(Path: p i)
     (if (name-ref=? i k)
         (match o
           [(Empty:) -empty-obj]
           ;; the result is not from an annotation, so it isn't a NoObject
           [(NoObject:) -empty-obj]
           [(Path: p* i*) (make-Path (append p p*) i*)]
           [(? LExp? l) (if (null? p) l -empty-obj)]
           [_ (int-err "unrecognized new object in subst-object: ~a" t)])
         t)]
    [(? LExp? l) (LExp-path-map sub-o l)]
    [_ (int-err "unrecognized target object in subst-object: ~a" t)]))

;; Substitution of objects into a filter in a filter set
;; This is ψ+ [o/x] and ψ- [o/x]
(define/cond-contract (subst-filter f k o polarity)
  (-> Filter/c name-ref/c Object? boolean? Filter/c)
  (define (sub-f f) (subst-filter f k o polarity))
  (define (sub-o o*) (subst-object o* k o polarity))
  
  (match f
    [(ImpFilter: ant consq)
     (-imp (subst-filter ant k o (not polarity)) (sub-f consq))]
    [(AndFilter: fs) 
     (apply -and (map sub-f fs))]
    [(OrFilter: fs) (apply -or (map sub-f fs))]
    [(? SLI? s) 
     (SLI-path-map sub-o s)]
    [(Bot:) -bot]
    [(Top:) -top]
    [(or (TypeFilter: t (Path: p i))
         (NotTypeFilter: t (Path: p i)))
     (define maker (if (TypeFilter? f) -filter -not-filter))
     (cond
       [(name-ref=? i k)
        (match o
          [(Empty:) (if polarity -top -bot)]
          [_ (maker
              (subst-type t k o polarity)
              (-acc-path p o))])]
       ;[(index-free-in? k t) (if polarity -top -bot)] this line is wrong -amk
       [else f])]
    [(or (TypeFilter: t (? LExp? l))
         (NotTypeFilter: t (? LExp? l)))
     (define maker (if (TypeFilter? f) -filter -not-filter))
     (let ([l* (LExp-path-map sub-o l)])
       (if (Empty? l*)
           (if polarity -top -bot)
           (maker (subst-type t k o polarity)
                  l*)))]))

;; Determine if the object k occurs free in the given type
;(define (index-free-in? k a)
;  (let/ec
;   return
;   (define (for-object o)
;     (object-case (#:Type for-type)
;                  o
;                  [#:Path p i
;                          (if (name-ref=? i k)
;                              (return #t)
;                              o)]))
;   (define (for-type t)
;     (type-case (#:Type for-type #:Object for-object #:Filter for-filter)
;       t
;       [#:arr dom rng rest drest kws dep?
;              (let* ([st* (if (pair? k)
;                              (λ (t) (index-free-in? (add-scope k) t))
;                              for-type)])
;                (for-each for-type dom)
;                (st* rng)
;                (and rest (for-type rest))
;                (and drest (for-type (car drest)))
;                (for-each for-type kws)
;                ;; dummy return value
;                (make-arr* null Univ #:dep? dep?))]
;       [#:InstdRef type prop
;              (let* ([st* (if (pair? k)
;                              (λ (t) (index-free-in? (add-scope k) t))
;                              for-type)]
;                     [sf* (if (pair? k)
;                              (λ (f) (index-free-in? (add-scope k) f))
;                              for-filter)])
;                (st* type)
;                (sf* prop))]))
;    (define (for-filter f)
;      (filter-case (#:Type for-type #:Object for-object #:Filter for-filter)
;                   f))
;   (if (Type? a)
;       (for-type a)
;       (for-filter a))
;    #f))
