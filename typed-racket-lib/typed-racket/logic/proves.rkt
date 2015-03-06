#lang racket/base
(require (except-in "../utils/utils.rkt" infer)
         racket/match racket/function racket/lazy-require racket/list unstable/function
         (except-in racket/contract ->* -> )
         (prefix-in c: (contract-req))
         (utils tc-utils)
         (env lookup type-env-structs)
         (logic prop-ops)
         (rep type-rep object-rep filter-rep rep-utils)
         (typecheck tc-metafunctions)
         (except-in "../types/abbrev.rkt" one-of/c))

(lazy-require
 ("../types/remove-intersect.rkt" (overlap))
 ("../types/path-type.rkt" (path-type))
 ("../types/filter-ops.rkt" (-and -or))
 ("../typecheck/tc-envops.rkt" (update))
 ("../types/subtype.rkt" (subtype)))

(provide proves witnesses update-env/type)

(define/cond-contract (proves A env obj new-props goal)
  (c:-> any/c env? Path? (listof Filter/c) Filter/c
        any/c)
  (let/ec exit*
    (define (exit) (exit* A))
    (define-values (compound-props atoms) 
      (combine-props (apply append (map flatten-nested-props new-props)) 
                     (env-props env) 
                     exit))
    (define env* 
      (for/fold ([Γ (replace-props env '())]) ([f (in-list atoms)])
        (match f
          [(or (TypeFilter: t obj) (NotTypeFilter: t obj))
           (update-env/type Γ obj t (TypeFilter? f) exit)]
          [_ Γ])))
    (define goal* (apply -and (logical-reduce A env* obj goal)))
    
    (cond
      [(Top? goal*) A]
      [else (and (full-proves A env* obj compound-props goal) A)])))

;;returns a list of the remaining goals to be proved
;; only proves based on type-env lookups
;; A env obj goal -> filter w/ proven facts removed
(define/cond-contract (logical-reduce A env obj goal)
  (c:-> any/c env? Path? Filter/c
        (listof Filter/c))
  (match goal
  
    [(Bot:) (list goal)]
    
    [(Top:) null]
    
    [(or (? TypeFilter?) (? NotTypeFilter?))
     (if (witnesses A env obj goal)
         null
         (list goal))]
    
    [(AndFilter: fs) 
     (let* ([fs* (apply append (map (curry logical-reduce A env obj) fs))]
            [f* (apply -and fs*)])
       (if (Top? f*)
           (list)
           (list f*)))]
    
    [(OrFilter: fs)
     (let* ([fs* (map (λ (f) (apply -and (logical-reduce A env obj f))) fs)]
            [f* (apply -or fs*)])
       (if (Top? f*)
           null
           (list f*)))]
    [_ (int-err "invalid goal: ~a" goal)]))


(define/cond-contract (full-proves A env obj assumptions goal)
  (c:-> any/c env? Path? (listof Filter/c) Filter/c
        boolean?)
  (match assumptions
    ['() (empty? (logical-reduce A env obj goal))]
    [(cons p ps)
     (match p
       [(Bot:) #t]
       
       [(Top:) (full-proves A env obj ps goal)]
       
       [(or (TypeFilter: t obj) 
            (NotTypeFilter: t obj))
        (define env* (update-env/type env obj t (TypeFilter? p) (λ () #f)))
        (define goal* (and env* (apply -and (logical-reduce A env* obj goal))))
        (or (not env*)
            (full-proves A env* obj ps goal*))]
       
       [(AndFilter: fs) (full-proves A env obj (append fs ps) goal)]
       
       ;; potential but unavoidable(?) performance ouch
       [(OrFilter: fs) (for/and ([f (in-list fs)]) 
                         (full-proves A env obj (cons f ps) goal))]
       [_ (int-err "invalid prop in assumptions: ~a" p)])]
    [_ (int-err "invalid assumption list: ~a" assumptions)]))

;; TODO(AMK) usage of ¬Type properties is still not complete


;; TODO proves will not consider aliasing
;; but... should it? who should? either the 
;; sight generating props to prove,
;; or we have to here


(define/cond-contract (witnesses A env obj goal)
  (c:-> any/c env? Path? (or/c TypeFilter? NotTypeFilter?)
        any/c)
  (match goal
    [(TypeFilter: ft (Path: π x)) 
     (let ([ty (lookup-id-type x env #:fail (λ (_) Univ))])
       (subtype (path-type π ty) ft #:A A #:env env #:obj obj))]
    [(NotTypeFilter: ft (Path: π x))
     (let ([ty (lookup-id-type x env #:fail (λ (_) Univ))]) 
       (not (overlap (path-type π ty) ft)))]
    [_ (int-err "invalid witnesses goal ~a" goal)]))



;; TODO(AMK) 
;; there are more complex refinement cases to consider such as 
;; 1. what about updating refinements that cannot be deconstructed? (i.e. nested
;;    inside other types?)
(define/cond-contract (update-env/type env obj type positive? bottom-k)
  (c:-> env? Object? Type/c boolean? procedure?
        (c:or/c env? #f))
  
  (match obj
    [(Path: π x) 
     (let* ([ty (lookup-id-type x env #:fail (λ (_) Univ))]
            [new-ty (update ty type positive? π)])
       (if (type-equal? new-ty -Bottom)
           (bottom-k)
           (naive-extend/type env x new-ty)))]
      [_ (int-err "invalid update-env obj prop: ~a" obj)]))
