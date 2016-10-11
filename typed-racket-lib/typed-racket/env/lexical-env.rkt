#lang racket/base

;; this environment maps *lexical* variables to types
;; it also contains the proposition environment

;; these environments are unified in "Logical Types for Untyped Languages"
;; but split here for performance

(require "../utils/utils.rkt"
         racket/keyword-transform racket/list
         (for-syntax syntax/parse racket/base)
         (contract-req)                       
         (env type-env-structs global-env)
         (utils tc-utils)
         (only-in (rep type-rep) Type?)
         (typecheck renamer)
         (except-in (types utils abbrev kw-types) -> ->* one-of/c))

(require-for-cond-contract (rep object-rep core-rep))

(provide lexical-env 
         with-lexical-env 
         with-lexical-env/extend-types
         with-lexical-env/extend-types+aliases)
(provide/cond-contract
 [lookup-type/lexical ((identifier?) (env? #:fail (or/c #f (-> any/c #f))) . ->* . (or/c Type? #f))]
 [lookup-alias/lexical ((identifier?) (env?) . ->* . (or/c Path? Empty?))])

;; the current lexical environment
(define lexical-env (make-parameter empty-prop-env))

;; run code in a new env
(define-syntax-rule (with-lexical-env e . b)
  (parameterize ([lexical-env e]) . b))

;; run code in an extended env
(define-syntax-rule (with-lexical-env/extend-types is ts . b)
  (with-lexical-env (extend/values (lexical-env) is ts) . b))

;; run code in an extended env + an alias extension
(define-syntax-rule (with-lexical-env/extend-types+aliases is ts os . b)
  (with-lexical-env (extend+alias/values (lexical-env) is ts os) . b))

;; find the type of identifier i, looking first in the lexical env, then in the top-level env
;; identifier -> Type
(define (lookup-type/lexical i [env (lexical-env)] #:fail [fail #f])
  (lookup env i (λ (i) (lookup-type i (λ () 
                                        (cond 
                                          [(syntax-property i 'constructor-for)
                                           => (λ (prop)
                                                (define orig (un-rename prop))
                                                (define t (lookup-type/lexical orig env))
                                                (register-type i t)
                                                t)]
                                          [(syntax-procedure-alias-property i) 
                                           => (λ (prop)
                                                (define orig (car (flatten prop)))
                                                (define t (lookup-type/lexical orig env))
                                                (register-type i t)
                                                t)]
                                          [(syntax-procedure-converted-arguments-property i)
                                           => (λ (prop)
                                                (define orig (car (flatten prop)))
                                                (define pre-t
                                                  (lookup-type/lexical 
                                                   orig env #:fail (lambda (i) (lookup-fail i) #f)))
                                                (define t (if pre-t
                                                              (kw-convert pre-t #f)
                                                              Err))
                                                (register-type i t)
                                                t)]
                                          [else ((or fail lookup-fail) i)]))))))

;; looks up the representative object for an id (i.e. itself or an alias if one exists)
(define (lookup-alias/lexical i [env (lexical-env)])
  (lookup-alias env i -id-path))