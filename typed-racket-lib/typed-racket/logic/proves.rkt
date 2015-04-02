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
 ("../types/path-type.rkt" (path-type unpath-type))
 ("../types/filter-ops.rkt" (-and -or))
 ("../types/numeric-tower.rkt" (integer-type))
 ("../typecheck/tc-envops.rkt" (update))
 ("../types/subtype.rkt" (subtype))
 ("../types/union.rkt" (Un)))

(provide proves witnesses update-env/atom simple-proves update-env/obj-type)

(define Bottom (Un))

(define (simple-proves axioms goal)
  (proves null empty-env axioms goal))

(define/cond-contract (proves A env new-props goal)
  (c:-> any/c env? (listof Filter/c) Filter/c
        any/c)
  (let/ec exit*
    (define (exit) (exit* A))
    (define-values (compound-props atoms slis)
      (combine-props (apply append (map flatten-nested-props new-props)) 
                     (env-props+SLIs env)
                     exit))
    (define env* 
      (for/fold ([Γ (replace-props env slis)]) 
                ([f (in-list atoms)])
        (match f
          [(or (? TypeFilter?) (? NotTypeFilter?))
           (update-env/atom A Γ f exit)]
          [_ Γ])))
    (define goal* (apply -and (logical-reduce A env* goal)))
    
    (cond
      [(Top? goal*) A]
      [else (and (full-proves A env* compound-props goal) A)])))

;;returns a list of the remaining goals to be proved
;; only proves based on type-env lookups
;; A env obj goal -> filter w/ proven facts removed
(define/cond-contract (logical-reduce A env goal)
  (c:-> any/c env? Filter/c
        (listof Filter/c))
  (match goal
  
    [(Bot:) (list goal)]
    
    [(Top:) null]
    
    [(or (? TypeFilter?) (? NotTypeFilter?))
     (if (witnesses A env goal)
         null
         (list goal))]
    
    [(? SLI? s)
     (if (SLIs-imply? (env-SLIs env) s)
         null
         (list goal))]
    
    [(AndFilter: fs)
     (let* ([fs* (apply append (map (curry logical-reduce A env) fs))]
            [f* (apply -and fs*)])
       (if (Top? f*)
           (list)
           (list f*)))]
    
    [(OrFilter: fs)
     (let* ([fs* (map (λ (f) (apply -and (logical-reduce A env f))) fs)]
            [f* (apply -or fs*)])
       (if (Top? f*)
           null
           (list f*)))]
    [_ (int-err "invalid goal: ~a" goal)]))

(define (atomic-prop? p)
         (or (Bot? p) (Top? p) (TypeFilter? p) (NotTypeFilter? p)))

(define/cond-contract (full-proves A env assumptions goal)
  (c:-> any/c env? (listof Filter/c) Filter/c
        boolean?)
  (match assumptions
    ['() (null? (logical-reduce A env goal))]
    [(cons p ps)
     (match p
       [(? atomic-prop?)
        (define env* (update-env/atom A env p (λ () #f)))
        (define goal* (and env* (apply -and (logical-reduce A env* goal))))
        (or (not env*)
            (full-proves A env* ps goal*))]
       
       [(? SLI? s)
        (define slis* (add-SLI s (env-SLIs env)))
        (define env* (if (Bot? slis*) #f (replace-props env (append (env-props env) slis*))))
        (define goal* (and env* (apply -and (logical-reduce A env* goal))))
        (or (not env*)
            (full-proves A env* ps goal*))]
       
       [(AndFilter: fs) (full-proves A env (append fs ps) goal)]
       
       ;; potential but unavoidable(?) performance ouch
       [(OrFilter: fs) (for/and ([f (in-list fs)]) 
                         (full-proves A env (cons f ps) goal))]
       [_ (int-err "invalid prop in assumptions: ~a" p)])]
    [_ (int-err "invalid assumption list: ~a" assumptions)]))

;; TODO(AMK) usage of ¬Type properties is still not complete

(define/cond-contract (witnesses A env goal)
  (c:-> any/c env? (or/c TypeFilter? NotTypeFilter?)
        any/c)
  (match goal
    [(TypeFilter: ft (and o (Path: π x)))
     (let ([ty (lookup-id-type x env #:fail (λ (_) Univ))])
       (subtype (path-type π ty) ft #:A A #:env env #:obj o))]
    
    [(NotTypeFilter: ft (and o (Path: π x)))
     (let ([x-ty+ (lookup-id-type x env #:fail (λ (_) Univ))]
           [x-ty- (lookup-id-not-type x env #:fail (λ (_) Bottom))]
           [goal-x-ty- (path-type π ft)])
       (or (subtype goal-x-ty- x-ty- #:A A #:env env #:obj o)
           (not (overlap x-ty+ goal-x-ty-))))]
    
    ;;TODO(amk) These should take into account the ranges
    ;; implied by the integer numeric-type when possible
    [(TypeFilter: ft (? LExp? l))
     (subtype ft (integer-type) #:A A #:env env #:obj l)]
    [(NotTypeFilter: ft (? LExp? l))
     (not (overlap (integer-type) ft))]
    [_ (int-err "invalid witnesses goal ~a" goal)]))



;; remove from the negative type info types that are impossible
;; based on the current positive facts in the environment
(define/cond-contract (update-negative-type ty+ ty-)
  (c:-> Type? Type? Type?)
  (match ty-
    [(Union: ts)
     (apply Un (filter (curry overlap ty+) ts))]
    [else (if (overlap ty+ ty-)
              ty-
              Bottom)]))

(define (update-env/obj-type env o t contradiction)
  (update-env/type+ null env t o contradiction))

(define/cond-contract (update-env/type+ A env t o contradiction)
  (c:-> any/c env? Type? Object? (c:or/c #f procedure?)
        (c:or/c env? #f))
  (match o
    [(Path: π x)
     (define x-ty+ (lookup-id-type x env #:fail (λ (_) Univ)))
     (define x-ty- (lookup-id-not-type x env #:fail (λ (_) Bottom)))
     (define new-x-ty+ (update (update x-ty+ t #t π) x-ty- #f null))
     (define new-x-ty- (update-negative-type new-x-ty+ x-ty-))
     (cond
       [(Bottom? new-x-ty+)
        (contradiction)]
       [(type-equal? new-x-ty- Univ)
        (contradiction)]
       [else (naive-extend/not-type (naive-extend/type env x new-x-ty+) x new-x-ty-)])]
    [(? LExp?)
     ;; TODO(amk) maybe do something more complex here with LExp and SLI info?
     (if (not (overlap (integer-type) t))
         (contradiction)
         env)]))

(define/cond-contract (update-env/type- A env t o contradiction)
  (c:-> any/c env? Type? Object? (c:or/c #f procedure?)
        (c:or/c env? #f))
  (match o
    [(Path: π x)
     (define x-ty+ (lookup-id-type x env #:fail (λ (_) Univ))) ;; x is of type T
     (define new-x-ty+ (update x-ty+ t #f π)) ;; combine new type-, x is now of type T'
     (define x-ty- (lookup-id-not-type x env #:fail (λ (_) Bottom))) ;; env says x is not of type T-
     (define new-x-ty- (update-negative-type new-x-ty+ (Un x-ty- (unpath-type π t Bottom))))
     (cond
       [(Bottom? new-x-ty+)
        ;(printf "\n/type- exiting w/ bottom\n")
        (contradiction)]
       [(type-equal? new-x-ty- Univ)
        ;(printf "\n/type- exiting w/ neg as Univ\n")
        (contradiction)]
       [else
        (naive-extend/not-type (naive-extend/type env x new-x-ty+) x new-x-ty-)])]
    [(? LExp?)
     ;; TODO(amk) maybe do something more complex here with LExp and SLI info?
     (if (subtype (integer-type) t #:A A #:env env #:obj o)
         (contradiction)
         env)]))

;; TODO(AMK) 
;; there are more complex refinement cases to consider such as 
;; 1. what about updating refinements that cannot be deconstructed? (i.e. nested
;;    inside other types inside of unions?)
(define/cond-contract (update-env/atom A env prop [contradiction #f])
  (c:-> any/c env? atomic-prop? (c:or/c #f procedure?)
        (c:or/c env? #f))
  
  (match prop
    [(? Top?) env]
    [(? Bot?) (and contradiction (contradiction))]
    [(TypeFilter: t o)
     (update-env/type+ A env t o (λ () (and contradiction (contradiction))))]
    [(NotTypeFilter: t o)
     (update-env/type- A env t o (λ () (and contradiction (contradiction))))]
    [_ (int-err "invalid update-env prop: ~a" prop)]))
