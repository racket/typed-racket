#lang racket/base

(require racket/match
         syntax/id-table
         (except-in "../utils/utils.rkt" env)
         (contract-req)
         ;; dict ops only used for convenient printing
         ;; (e.g. performance is irrelevant)
         (only-in racket/dict dict->list dict-map)
         (rep core-rep object-rep)
         (types numeric-tower)
         (for-syntax racket/base syntax/parse))

(require-for-cond-contract (rep type-rep prop-rep))

;; types is a free-id-table of identifiers to types
;; props is a list of known propositions
(define-struct/cond-contract env ([types immutable-free-id-table?]
                                  [obj-types (hash/c Object? Type? #:immutable #t)]
                                  [props (listof Prop?)]
                                  [aliases immutable-free-id-table?])
  #:transparent
  #:property prop:custom-write
  (lambda (e prt mode)
    (fprintf prt "(env ~a ~a ~a)"
             (dict-map (append (dict->list (env-types e))
                               (hash->list (env-obj-types e)))
                       (λ (id ty) (format "[~a ∈ ~a]" id ty)))
             (env-props e)
             (free-id-table-map (env-aliases e)
                                (λ (id ty) (format "[~a ≡ ~a]" id ty))))))

(provide/cond-contract
  [env? predicate/c]
  [env-set-id-type (env? identifier? Type? . -> . env?)]
  [env-set-obj-type (env? Object? Type? . -> . env?)]
  [env-extend/bindings (env? (listof identifier?)
                             (listof Type?)
                             (or/c (listof OptObject?) #f)
                             . -> .
                             env?)]
  [env-lookup-id (env? identifier? (identifier? . -> . any) . -> . any)]
  [env-lookup-obj (env? Object? (Object? . -> . any) . -> . any)]
  [env-props (env? . -> . (listof Prop?))]
  [env-replace-props (env? (listof Prop?) . -> . env?)]
  [empty-env env?]
  [env-lookup-alias (env? identifier? (identifier? . -> . (or/c OptObject? #f)) . -> . (or/c OptObject? #f))])


(provide lexical-env
         with-lexical-env
         with-naively-extended-lexical-env)

(define empty-env
  (env
    (make-immutable-free-id-table)
    (hash)
    null
    (make-immutable-free-id-table)))

(define lexical-env (make-parameter empty-env))

;; run code in a new env
(define-syntax-rule (with-lexical-env e . b)
  (parameterize ([lexical-env e]) . b))

;; "naively" extends the environment by simply adding the props
;; to the environments prop list (i.e. it doesn't do any logical
;; simplifications like env+ in tc-envops.rkt)
(define-syntax (with-naively-extended-lexical-env stx)
  (syntax-parse stx
    [(_ [(~optional (~seq #:identifiers ids-expr:expr
                          #:types ts-expr:expr)
                    #:defaults ([ids-expr #'(list)]
                                [ts-expr #'(list)]))
         (~optional (~seq #:props ps:expr)
                    #:defaults ([ps #'(list)]))]
        . body)
     (syntax/loc stx
       (let ([cur-env (lexical-env)]
             [ids ids-expr]
             [initial-ts ts-expr])
         (let-values ([(ts pss)
                       (for/lists (_1 _2)
                         ([id (in-list ids)]
                          [t (in-list initial-ts)])
                         (extract-props (-id-path id) t))])
           (with-lexical-env
               (env-replace-props
                (env-extend/bindings cur-env ids ts #f)
                (append (apply append pss)
                        ps
                        (env-props cur-env)))
             . body))))]))


(define (env-replace-props e props)
  (match-let ([(env tys otys _ als) e])
    (env tys otys props als)))

(define (env-lookup-id e key fail)
  (match-let ([(env tys _ _ _) e])
    (free-id-table-ref tys key (λ () (fail key)))))

(define (env-lookup-obj e key fail)
  (match-let ([(env _ otys _ _) e])
    (hash-ref otys key (λ () (fail key)))))


;; like hash-set, but for the type of an ident in the lexical environment
(define (env-set-id-type e ident type)
  (match-let ([(env tys otys ps als) e])
    (env (free-id-table-set tys ident type) otys ps als)))

;; like hash-set, but for the type of an object in the lexical environment
(define (env-set-obj-type e obj type)
  (match-let ([(env tys otys ps als) e])
    (env tys (hash-set otys obj type) ps als)))

;; extends an environment with types and aliases
;; e : the 'env' struct to be updated
;; idents : the identifiers which are getting types (or aliases)
;; types : the types for the 'idents'
;; aliased-objs : what object is each x ∈ 'idents' an alias for
;;               (Empty means it does not alias anything). If
;;               there are no aliases, you can pass #f
;;               for 'aliased-objs' to simplify the computation.
(define (env-extend/bindings e idents types maybe-aliased-objs)
  (match-define (env tys otys ps als) e)
  ;; NOTE: we mutate the identifiers 'tys' and 'als' for convenience,
  ;; but the free-id-tables themselves are immutable.
  (define (update/type! id t)
    (set! tys (free-id-table-set tys id t)))
  (define (update/obj-type! lexp t)
    (set! otys (hash-set otys lexp t)))
  (define (update/alias! id o)
    (set! als (free-id-table-set als id o)))
  (define (update/type+alias! id t o)
    (match o
      ;; no alias, so just record the type as usual
      [(Empty:) (update/type! id t)]
      ;; 'id' is aliased to the identifier 'id*';
      ;; record the alias relation 'id ≡ id*' *and* that 'id* ∈ t'
      [(Path: '() id*) (update/type! id* t)
                       (update/alias! id o)]
      ;; id is aliased to a Path w/ a non-empty path element list;
      ;; just record the alias.
      [(? Path? p) (update/alias! id p)]
      ;; for ids that alias non-Path objs, we record the type and alias
      [o (update/obj-type! o t)
         (update/alias! id o)]))
  (if maybe-aliased-objs
      (for-each update/type+alias! idents types maybe-aliased-objs)
      (for-each update/type! idents types))
  (env tys otys ps als))

(define (env-lookup-alias e key fail)
  (match-let ([(env _ _ _ als) e])
    (free-id-table-ref als key (λ () (fail key)))))

