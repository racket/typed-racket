#lang racket/base

(require (except-in "../utils/utils.rkt" infer)
         racket/match racket/function racket/lazy-require racket/list unstable/function
         (except-in racket/contract ->* -> )
         (prefix-in c: (contract-req))
         (utils tc-utils)
         (logic proves)
         (env lookup lexical-env)
         (rep type-rep object-rep filter-rep rep-utils)
         (except-in "../types/abbrev.rkt" one-of/c))

 (lazy-require
  ("../types/remove-intersect.rkt" (overlap))
  ("../types/path-type.rkt" (path-type))
  ("../types/subtype.rkt" (subtype))
  ("../types/filter-ops.rkt" (-and -or))
  ("../typecheck/tc-envops.rkt" (env-extend-types)))

(provide type-update update-function/arg-types)

(define (type-update ty env)
  (define (do-type ty)
    (type-case
     (#:Type do-type #:Filter do-filter #:Object do-obj)
     ty))
  
  (define (do-filter f)
    (filter-case (#:Type do-type
                  #:Filter do-filter
                  #:Object do-obj)
                 f
                 [#:TypeFilter t o 
                  (let ([ty (lookup-obj-type o env #:fail (λ (_) #f))])
                    (cond
                      [(and ty (not (overlap t ty))) -bot]
                      [else f]))]
                 [#:NotTypeFilter t o
                  (let ([ty (lookup-obj-type o env #:fail (λ (_) #f))])
                    (cond
                      [(and ty (subtype t ty #:env env #:obj o)) -bot]
                      [else f]))]
                 [#:AndFilter fs (apply -and (map do-filter fs))]
                 [#:OrFilter fs (apply -or (map do-filter fs))]))
  
  (define (do-obj o)
    (object-case (#:Type do-type
                  #:Object do-obj
                  #:PathElem do-pe)
                 o))
  
  (define (do-pe pe)
    (pathelem-case (#:Type do-type
                    #:PathElem do-pe)
                   pe))
  (do-type ty))



(define (update-function/arg-types arg-tys f-type)
  ;; TODO support polymorphic functions?
  ;; e.g. match-define: no matching clause for (All (a) (-> (Listof a) Index))
  ;; if match-define (Function: (list (arr: domss rngs rests drests kwss dep?s) ...))
  (match f-type
    [(Function: (list (arr: domss rngs rests drests kwss dep?s) ...)) 
     #:when (ormap (λ (x) x) dep?s)
     (define new-arrs 
       (for/list ([doms domss]
                  [rng rngs]
                  [rest rests]
                  [drest drests]
                  [kws kwss]
                  [dep? dep?s])
         (cond
           [(not dep?)
            (make-arr doms rng rest drest kws dep?)]
           [else
            (let*-values
                ([(ids) (genids (length doms) 'arg)]
                 [(doms rng) ((instantiate-many ids) doms rng)]
                 [(env) (env-extend-types (lexical-env) ids arg-tys)]
                 [(doms) (if env
                             (map (curryr type-update env) doms)
                             doms)]
                 [(rng) (if env
                            (type-update rng env)
                            -Bottom)]
                 [(doms) (map (curry abstract-idents ids) doms)]
                 [(rng) (abstract-idents ids rng)])
              (make-arr doms rng rest drest kws dep?))])))
     (make-Function new-arrs)]
    [_ f-type]))
