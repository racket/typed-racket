#lang racket/base

(require "../utils/utils.rkt"
         racket/match racket/lazy-require racket/keyword-transform racket/list
         (except-in racket/contract ->* -> )
         (env type-env-structs global-env mvar-env)
         (utils tc-utils)
         (rep object-rep rep-utils object-ops)
         (only-in (rep type-rep) Type/c)
         (typecheck renamer)
         (prefix-in c: (contract-req))
         (except-in (types utils abbrev) -> ->* one-of/c))


(lazy-require 
 ("../types/kw-types.rkt" (kw-convert))
 ("../types/numeric-tower.rkt" (integer-type))
 ("../types/path-type.rkt" (path-type))
 ("../typecheck/typechecker.rkt" (tc-literal)))

(provide lookup-id-type lookup-id-not-type lookup-obj-type lookup-obj-not-type resolve-id-alias)

(define/cond-contract (lookup-id-type id env #:fail [fail #f])
  (c:->* (identifier? env?) (#:fail (c:or/c #f (c:-> any/c (c:or/c Type/c #f))))
         (c:or/c Type/c #f))
  
  ;; resolve any alias, lookup/calculate type
  (define-values (π* id* id*-ty)
    (match (resolve-id-alias id env)
      [(Path: π x)
       (let ([x-ty (env-struct-lookup x env #:fail fail)])
         (values π x x-ty))]
      [(? LExp? l)
       (cond
         [(constant-LExp? l) 
          => (λ (c) (values null l (tc-literal (datum->syntax #f c))))]
         ;; TODO(amk) might be able to leverage more info about the LExp here?
         [else (values null l (integer-type))])]
      [(Empty:) (values null id (env-struct-lookup id env #:fail fail))]))
  
  
  (cond
    [id*-ty (if (null? π*) 
            id*-ty 
            (path-type π* id*-ty))]
    [fail (fail id)]
    [else (lookup-fail id)]))

(define/cond-contract (lookup-id-not-type id env #:fail [fail #f])
  (c:->* (identifier? env?) (#:fail (c:or/c #f (c:-> any/c (c:or/c Type/c #f))))
         (c:or/c Type/c #f))
  
  ;; resolve any alias, lookup/calculate type
  (define-values (π* id* id*-not-ty)
    (match (resolve-id-alias id env)
      [(Path: π x)
       (let ([x-not-ty (raw-lookup-not-type env x fail)])
         (values π x x-not-ty))]
      [(? LExp? l)
       (values null id #f)]
      [(Empty:) (values null id (raw-lookup-not-type env id fail))]))
  
  
  (cond
    [id*-not-ty (if (null? π*)
                    id*-not-ty 
                    (path-type π* id*-not-ty))]
    [fail (fail id)]
    [else -Bottom]))

(define/cond-contract (lookup-obj-not-type o env #:fail [fail #f])
  (c:->* (Object? env?) (#:fail (c:or/c #f (c:-> any/c (c:or/c Type/c #f))))
         (c:or/c Type/c #f))
  (match o
    [(Path: π (? identifier? x))
     (let ([ty (lookup-id-not-type x env #:fail fail)])
       (cond 
         [ty (path-type π ty)]
         [fail (fail o)]
         [else -Bottom]))]
    ;; ignore LExps specifics?
    [_ #:when fail (fail o)]
    [_ -Bottom]))


(define/cond-contract (lookup-obj-type o env #:fail [fail #f])
  (c:->* (Object? env?) (#:fail (c:or/c #f (c:-> any/c (c:or/c Type/c #f))))
         (c:or/c Type/c #f))
  (match o
    [(Path: π (? identifier? x)) 
     (let ([ty (lookup-id-type x env #:fail fail)])
       (cond 
         [ty (path-type π ty)]
         [fail (fail o)]
         [else (lookup-fail o)]))]
    ;; TODO(amk) maybe something else here more specific
    ;; for what LExp it is? I dunno
    [(? LExp?) (integer-type)]
    [_ #:when fail (fail o)]
    [_ (lookup-fail o)]))

(define/cond-contract (resolve-id-alias id env)
  (c:-> identifier? env? Object?)
  (raw-lookup-alias env id -id-path))


(define/cond-contract (env-struct-lookup i env #:fail [fail #f])
  (c:->* (identifier? env?) (#:fail (c:or/c #f (c:-> any/c (c:or/c Type/c #f))))
       (c:or/c Type/c #f))
  (raw-lookup-type env i (λ (i) (lookup-type i (λ ()
                                        (cond 
                                          [(syntax-property i 'constructor-for)
                                           => (λ (prop)
                                                (define orig (un-rename prop))
                                                (define t (env-struct-lookup orig env))
                                                (register-type i t)
                                                t)]
                                          [(syntax-procedure-alias-property i) 
                                           => (λ (prop)
                                                (define orig (car (flatten prop)))
                                                (define t (env-struct-lookup orig env))
                                                (register-type i t)
                                                t)]
                                          [(syntax-procedure-converted-arguments-property i)
                                           => (λ (prop)
                                                (define orig (car (flatten prop)))
                                                (define pre-t
                                                  (env-struct-lookup 
                                                   orig env #:fail (lambda (i) (lookup-fail i) #f)))
                                                (define t (if pre-t
                                                              (kw-convert pre-t #f)
                                                              Err))
                                                (register-type i t)
                                                t)]
                                          [else ((or fail lookup-fail) i)]))))))
