#lang racket/base

(require "../utils/utils.rkt"
         racket/match racket/lazy-require racket/keyword-transform racket/list
         (except-in racket/contract ->* -> )
         (env type-env-structs global-env mvar-env)
         (utils tc-utils)
         (rep object-rep rep-utils)
         (only-in (rep type-rep) Type/c)
         (typecheck renamer)
         (prefix-in c: (contract-req))
         (except-in (types utils abbrev) -> ->* one-of/c))


(lazy-require 
 ("../types/kw-types.rkt" (kw-convert))
 ("../types/path-type.rkt" (path-type)))

(provide lookup-id-type lookup-obj-type resolve-id-alias)

(define/cond-contract (lookup-id-type id env #:fail [fail #f])
  (c:->* (identifier? env?) (#:fail (c:or/c #f (c:-> any/c (c:or/c Type/c #f))))
         (c:or/c Type/c #f))
  (define obj (resolve-id-alias id env))
  (define-values (alias-path alias-id)
    (match obj
      [(Path: p x) (values p x)]
      [_ (values (list) id)]))
  
  (define ty (env-struct-lookup alias-id env #:fail fail))
  ;; calculate the type, resolving aliasing and paths if necessary
  (cond
    [ty (path-type alias-path ty)]
    [fail (fail id)]
    [else (lookup-fail id)]))

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
