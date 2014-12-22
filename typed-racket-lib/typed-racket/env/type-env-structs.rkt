#lang racket/base

(require racket/match
         syntax/id-table
         (except-in "../utils/utils.rkt" env)
         (contract-req)
         (rep object-rep))

(require-for-cond-contract (rep type-rep filter-rep))

;; types is a free-id-table of identifiers to types
;; props is a list of known propositions
(define-struct/cond-contract env ([types immutable-free-id-table?] 
                                  [props (listof Filter/c)]
                                  [aliases immutable-free-id-table?])
  #:transparent
  #:property prop:custom-write
  (lambda (e prt mode)
    (fprintf prt "(env ~a ~a)" (free-id-table-map (env-types e) list) (env-props e))))

(provide/cond-contract
  [env? predicate/c]
  [extend (env? identifier? Type/c . -> . env?)]
  [extend/values (env? (listof identifier?) (listof Type/c) . -> . env?)]
  [lookup (env? identifier? (identifier? . -> . any) . -> . any)]
  [env-props (env? . -> . (listof Filter/c))]
  [replace-props (env? (listof Filter/c) . -> . env?)]
  [empty-prop-env env?]
  [extend+alias/values (env? (listof identifier?) (listof Type/c) (listof Object?) . -> . env?)]
  [lookup-alias (env? identifier? (identifier? . -> . (or/c #f Object?)) . -> . (or/c #f Object?))])

(define empty-prop-env
  (env
    (make-immutable-free-id-table)
    null
    (make-immutable-free-id-table)))


(define (replace-props e props)
  (match-let ([(env tys _ als) e])
    (env tys props als)))

(define (lookup e key fail)
  (match-let ([(env tys _ _) e])
    (free-id-table-ref tys key (λ () (fail key)))))


;; extend that works on single arguments
(define (extend e k v)
  (extend/values e (list k) (list v)))

;; extends an environment with types (no aliases)
(define (extend/values e ks vs)
  (match-let* ([(env tys ps als) e]
               [tys* (for/fold ([tys tys]) ([k (in-list ks)] [v (in-list vs)])
                       (free-id-table-set tys k v))])
    (env tys* ps als)))

;; extends an environment with types and aliases
(define (extend+alias/values e ids ts os)
  (match-let*-values 
   ([((env tys ps als)) e]
    [(tys* als*) (for/fold ([tys tys]
                            [als als]) 
                           ([id (in-list ids)] 
                            [t (in-list ts)]
                            [o (in-list os)])
                   (match o
                     ;; no alias, so just record the type as usual
                     [(Empty:)
                      (values (free-id-table-set tys id t) als)]
                     ;; id is aliased to an identifier
                     [(Path: '() id*)
                      ;; record the alias relation *and* type of that alias id
                      (values (free-id-table-set tys id* t) 
                              (free-id-table-set als id o))]
                     ;; id is aliased to an object with a non-empty path
                     [(Path: p x)
                      ;; just record the alias
                      (values tys (free-id-table-set als id o))]))])
   (env tys* ps als*)))

(define (lookup-alias e key fail)
  (match-let ([(env _ _ als) e])
    (free-id-table-ref als key (λ () (fail key)))))

