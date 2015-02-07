#lang racket/base

;; this environment maps *lexical* variables to types
;; it also contains the proposition environment

;; these environments are unified in "Logical Types for Untyped Languages"
;; but split here for performance

(require "../utils/utils.rkt"
         racket/keyword-transform racket/list racket/lazy-require
         (for-syntax syntax/parse racket/base)
         (contract-req)                       
         (env lookup global-env mvar-env type-env-structs)
         (utils tc-utils)
         (only-in (rep type-rep) Type/c)
         (typecheck renamer)
         (except-in (types utils abbrev) -> ->* one-of/c))

(require-for-cond-contract (rep object-rep))

(provide lexical-env 
         with-lexical-env 
         update-type/lexical)
(provide/cond-contract
 [lookup-type/lexical ((identifier?) (env? #:fail (or/c #f (-> any/c (or/c Type/c #f)))) 
                                     . ->* . (or/c Type/c #f))]
 [resolve-alias/lexical ((identifier?) (env?) . ->* . (or/c Path? Empty?))])

;; the current lexical environment
(define lexical-env (make-parameter empty-env))

;; run code in a new env
(define-syntax-rule (with-lexical-env e . b)
  (parameterize ([lexical-env e]) . b))


;; find the type of identifier i, looking first in the lexical env, then in the top-level env
;; identifier -> Type
(define (lookup-type/lexical i [env (lexical-env)] #:fail [fail #f])
  (lookup-id-type i env #:fail fail))

(define (resolve-alias/lexical i [env (lexical-env)])
  (resolve-id-alias i env))

;; refine the type of i in the lexical env
;; (identifier type -> type) identifier -> environment
;; a macro for inlining :(
(define-syntax (update-type/lexical stx)
  (syntax-parse stx
    [(_ f i env)
     #:declare f (expr/c #'(identifier? Type/c . -> . Type/c))
     #:declare i (expr/c #'identifier?)
     #:declare env (expr/c #'prop-env?)
     ;; check if i is ever the target of a set!
     ;; or is a top-level variable
     #'(if (or (is-var-mutated? i)
               (not (identifier-binding i)))
           ;; if it is, we do nothing
           env
           ;; otherwise, refine the type
           (parameterize
               ([current-orig-stx i])
             (let* ([v (lookup-type/lexical i env #:fail (lambda _ Univ))]
                    [new-v (f i v)]
                    [new-env (naive-extend/type env i new-v)])
               new-env)))]))
