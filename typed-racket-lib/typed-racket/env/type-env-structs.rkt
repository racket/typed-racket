#lang racket/base

(require racket/match
         syntax/id-table
         (except-in "../utils/utils.rkt" env)
         (contract-req)
         (rep object-rep type-rep rep-utils filter-rep))

(require-for-cond-contract (rep type-rep))

;; types is a free-id-table of identifiers to types
;; props is a list of known propositions
(define-struct/cond-contract env ([types immutable-free-id-table?] 
                                  [props (listof Filter/c)]
                                  [aliases immutable-free-id-table?]
                                  [sli (or/c #f SLI?)])
  #:transparent
  #:property prop:custom-write
  (lambda (e prt mode)
    (fprintf prt "(env ~a ~a)" (free-id-table-map (env-types e) list) (env-props e))))

(provide/cond-contract
  [env? predicate/c]
  [raw-lookup-type (env? identifier? (identifier? . -> . any) . -> . any)]
  [env-props (env? . -> . (listof Filter/c))]
  [env-SLI (env? . -> . (or/c #f SLI?))]
  [replace-props (env? (listof Filter/c) . -> . env?)]
  [empty-env env?]
  [raw-lookup-alias (env? identifier? (identifier? . -> . (or/c #f Object?)) . -> . (or/c #f Object?))]
  [env-extract-props (env? . -> . (values env? (listof Filter/c)))]
  [naive-extend/type (env? identifier? Type? . -> . env?)]
  [naive-extend/types (env? (listof (cons/c identifier? (and/c Type?
                                                               (not/c Bottom?)))) 
                            . -> . env?)]
  [extend/aliases (env? (listof (cons/c identifier? (and/c Object?
                                                           (not/c Empty?)))) 
                        . -> . env?)]
  [env-SLI-satisfiable? (env? . -> . boolean?)])


(define empty-env
  (env
    (make-immutable-free-id-table)
    null
    (make-immutable-free-id-table)
    #f))

(define (env-SLI e)
  (match-let ([(env _ _ _ sli) e])
    sli))

(define (env-extract-props e)
  (match-let ([(env tys fs als sli) e])
    (values (env tys (list) als sli) fs)))

(define (replace-props e props)
  (match-let ([(env tys _ als sli) e])
    (env tys props als sli)))

(define (replace-SLI e sli)
  (match-let ([(env tys props als _) e])
    (env tys props als sli)))

(define (raw-lookup-type e key fail)
  (match-let ([(env tys _ _ _) e])
    (free-id-table-ref tys key (λ () (fail key)))))

(define (raw-lookup-alias e key fail)
  (match-let ([(env _ _ als _) e])
    (free-id-table-ref als key (λ () (fail key)))))


;; extend that works on single arguments
(define (naive-extend/type e id type)
  (naive-extend/types e (list (cons id type))))

;; extends an environment with types (no aliases)
;; DOES NOT FLATTEN NESTED REFINEMENT TYPE PROPS
(define (naive-extend/types e ids/types)
  (match-let* ([(env tys ps als sli) e]
               [tys* (for/fold ([tys tys]) 
                               ([id/ty (in-list ids/types)])
                       (free-id-table-set tys (car id/ty) (cdr id/ty)))])
    (env tys* ps als sli)))

;; extends an environment with aliases
(define (extend/aliases e ids/aliases)
  (match-let* ([(env tys ps als sli) e]
               [als* (for/fold ([als als]) 
                               ([id/obj (in-list ids/aliases)])
                       (free-id-table-set als (car id/obj) (cdr id/obj)))])
    (env tys ps als* sli)))
