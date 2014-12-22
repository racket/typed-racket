#lang racket/base

;; this environment maps *lexical* variables to types
;; it also contains the proposition environment

;; these environments are unified in "Logical Types for Untyped Languages"
;; but split here for performance

(require "../utils/utils.rkt"
         racket/keyword-transform racket/list racket/match racket/set
         (for-syntax syntax/parse racket/base)
         (contract-req)
         (env type-env-structs global-env mvar-env)
         (rep object-rep type-rep rep-utils)
         (utils tc-utils)
         (only-in (rep type-rep) Type/c)
         (typecheck renamer)
         (types subtype resolve union)
         (except-in (types utils abbrev kw-types) -> ->* one-of/c))

(provide lexical-env 
         with-lexical-env 
         with-lexical-env/extend-types
         with-lexical-env/extend-types+aliases
         update-type/lexical)
(provide/cond-contract
 [lookup-type/lexical ((identifier?) (env? #:fail (or/c #f (-> any/c #f))) . ->* . (or/c Type/c #f))]
 [lookup-alias/lexical ((identifier?) (env?) . ->* . (or/c Path? Empty?))]
 [path-type ((listof PathElem?) Type/c . -> . Type/c)])

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
                    [new-env (extend env i new-v)])
               new-env)))]))

;; returns the result of following a path into a type
;; (Listof PathElem) Type -> Type
;; Ex. '(CarPE) (Pair α β) -> α
;; resolved is the set of resolved types so far at a particular
;; path - it ensures we are making progress, that we do not
;; continue unfolding types infinitely while not progressing.
;; It is intentionally reset each time we decrease the
;; paths size on a recursive call, and maintained/extended
;; when the path does not decrease on a recursive call.
(define (path-type path t [resolved (set)])
  (match* (t path)
    ;; empty path
    [(t (list)) t]
    
    ;; pair ops
    [((Pair: t s) (list rst ... (CarPE:)))
     (path-type rst t)]
    [((Pair: t s) (list rst ... (CdrPE:)))
     (path-type rst s)]

    ;; syntax ops
    [((Syntax: t) (list rst ... (SyntaxPE:)))
     (path-type rst t)]

    ;; promise op
    [((Promise: t) (list rst ... (ForcePE:)))
     (path-type rst t)]

    ;; struct ops
    [((Struct: nm par flds proc poly pred)
      (list rst ... (StructPE: (? (λ (s) (subtype t s)) s) idx)))
     (match-let ([(fld: ft _ _) (list-ref flds idx)])
       (path-type rst ft))]
    
    [((Union: ts) _)
     (apply Un (map (λ (t) (path-type path t resolved)) ts))]
    
    ;; paths into polymorphic types
    [((Poly: _ body-t) _) (path-type path body-t resolved)]
    [((PolyDots: _ body-t) _) (path-type path body-t resolved)]
    [((PolyRow: _ _ body-t) _) (path-type path body-t resolved)]
    
    ;; types which need resolving
    [((? needs-resolving?) _) #:when (not (set-member? resolved t))
     (path-type path (resolve-once t) (set-add resolved t))]
    
    ;; type/path mismatch =(
    [(_ _)
     (int-err "\n\tBad path/type lookup!\n\tPath:~a\n\tType: ~a\n" path t)]))
