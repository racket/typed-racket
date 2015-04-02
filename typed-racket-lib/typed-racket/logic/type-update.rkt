#lang racket/base

(require (except-in "../utils/utils.rkt" infer)
         racket/match racket/function racket/lazy-require racket/list unstable/function
         (except-in racket/contract ->* -> )
         (prefix-in c: (contract-req))
         (utils tc-utils)
         (logic proves)
         (env lookup lexical-env type-env-structs)
         (rep type-rep object-rep filter-rep rep-utils)
         (types tc-result)
         (typecheck tc-subst)
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
                 [#:OrFilter fs (apply -or (map do-filter fs))]
                 [#:SLI sli
                        (let ([env-slis (env-SLIs env)])
                          (cond
                            [(SLIs-imply? env-slis sli) -top]
                            [(Bot? (add-SLI sli env-slis)) -bot]
                            [else sli]))]))
  
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



(define (update-function/arg-types args-res f-type)
  ;; TODO support polymorphic functions
  ;; e.g. match-define: no matching clause for (All (a) (-> (Listof a) Index))
  ;; if match-define (Function: (list (arr: domss rngs rests drests kwss dep?s) ...))
  ;; grab objects for the arguments if there is one
  (define-values (arg-tys arg-objs)
    (for/lists (ts os)
               ([tcres (in-list args-res)])
      (match tcres
        [(tc-result1: t _ o)
         (values t (and (or (Path? o) (LExp? o))
                        o))]
        [_ (int-err "unknown tc-result type for argument ~a" tcres)])))
  
  (match f-type
    [(Function: (list (and arrs (arr: domss rngs rests drests kwss dep?s)) ...)) 
     #:when (ormap (λ (x) x) dep?s)
     (define new-arrs 
       (for/list ([arr (in-list arrs)]
                  [doms (in-list domss)]
                  [rng (in-list rngs)]
                  [rest (in-list rests)]
                  [drest (in-list drests)]
                  [kws (in-list kwss)]
                  [dep? (in-list dep?s)])
         (cond
           [(not dep?) arr]
           [else
            (define tmp-ids (genids (length doms) 'arg))
            (define-values (tmp-doms tmp-rng)
              ((instantiate-many tmp-ids) doms rng))
            ;; replace tmp ids w/ objects when possible in domains
            (define doms*
              (for/list ([d (in-list tmp-doms)])
                (for/fold ([d* d])
                          ([o (in-list arg-objs)]
                           [id (in-list tmp-ids)])
                  (if o (subst-type d* id o #t) d*))))
            ;; replace tmp ids w/ objects when possible in the range
            (define rng*
              (for/fold ([r* tmp-rng])
                        ([o (in-list arg-objs)]
                         [o-ty (in-list arg-tys)]
                         [id (in-list tmp-ids)])
                (if o (subst-result r* id o #t o-ty) r*)))
            (let/ec exit*
              (define (exit) (exit* -Bottom))
              ;; update the lexical environment with domain types
              (define env*
                (for/fold ([env (lexical-env)])
                          ([tmp-id (in-list tmp-ids)]
                           [obj (in-list arg-objs)]
                           [ty (in-list doms*)])
                  (update-env/obj-type env
                                       (if obj obj (-id-path tmp-id))
                                       ty
                                       exit)))
              (define updated-doms (for/list ([d (in-list doms*)])
                                     (abstract-idents tmp-ids (type-update d env*))))
              (define updated-rng (abstract-idents tmp-ids (type-update rng* env*)))
              (make-arr updated-doms updated-rng rest drest kws dep?))])))
     (make-Function new-arrs)]
    [_ f-type]))
