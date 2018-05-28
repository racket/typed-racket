#lang racket/unit

(require "../../utils/utils.rkt"
         "signatures.rkt"
         "utils.rkt"
         (utils tc-utils)
         syntax/parse racket/match 
         syntax/parse/experimental/reflect
         "../signatures.rkt" "../tc-funapp.rkt"
         "../integer-refinements.rkt"
         (types abbrev utils prop-ops)
         (env lexical-env)
         (typecheck tc-subst tc-envops check-below)
         (rep type-rep prop-rep object-rep values-rep))

(import tc-expr^ tc-app-keywords^
        tc-app-hetero^ tc-app-list^ tc-app-apply^ tc-app-values^
        tc-app-objects^ tc-app-eq^ tc-app-lambda^ tc-app-special^
        tc-app-contracts^)
(export tc-app^)

(define-tc/app-syntax-class (tc/app-regular* expected)
  (pattern form (tc/app-regular #'form expected)))

(define-syntax-rule (combine-tc/app-syntax-classes class-name case ...)
  (define-syntax-class (class-name expected)
    #:attributes (check)
    (pattern (~reflect v (case expected) #:attributes (check))
             #:attr check (attribute v.check)) ...))

(combine-tc/app-syntax-classes tc/app-special-cases
  tc/app-list
  tc/app-apply
  tc/app-eq
  tc/app-hetero
  tc/app-values
  tc/app-keywords
  tc/app-objects
  tc/app-lambda
  tc/app-special
  tc/app-contracts
  tc/app-regular*)

;; the main dispatching function
;; syntax (or/c tc-results/c #f) -> tc-results/c
(define (tc/app form expected)
  (syntax-parse form
    [(#%plain-app . (~var v (tc/app-special-cases expected)))
     ((attribute v.check))]))

;; TODO: handle drest, and props/objects
(define (arrow-matches? arr args)
  (match arr
    [(Arrow: domain
             (and rst (not (? RestDots?)))
             (list (Keyword: _ _ #f) ...)
             (Values: (list (Result: v
                                     (PropSet: (TrueProp:) (TrueProp:))
                                     (Empty:))
                            ...)))
     (Arrow-includes-arity? domain rst args)]
    [_ #f]))

(define (has-props? arr)
  (match arr
    [(Arrow: _
             _
             (list (Keyword: _ _ #f) ...)
             (Values: (list (Result: v
                                     (PropSet: (TrueProp:) (TrueProp:))
                                     (Empty:)) ...)))
     #f]
    [else #t]))


(define (tc/app-regular form expected)
  (syntax-case form ()
    [(f . args)
     (let ([f-ty (tc-expr/t #'f)]
           [args* (syntax->list #'args)])
       (define (matching-arities arrs)
         (for/list ([arr (in-list arrs)] #:when (arrow-matches? arr args*)) arr))
       (define (has-drest/props? arrs)
         (for/or ([arr (in-list arrs)])
           (or (has-props? arr)
               (RestDots? (Arrow-rst arr)))))
       (match f-ty
         [(Fun: (? has-drest/props?))
          (define arg-types
            (cond
              [(with-refinements?)
               (map tc-dep-fun-arg args*)]
              [else (map single-value args*)]))
          (tc/funapp #'f #'args f-ty arg-types expected)]
         [(or (? DepFun?)
              (Poly-unsafe: _ (? DepFun?)))
          (parameterize ([with-refinements? #t])
            (tc/funapp #'f #'args f-ty (map tc-dep-fun-arg args*) expected))]
         [(Fun: (app matching-arities
                     (list (Arrow: doms rsts _ _) ..1)))
          (define check-arg (if (and (identifier? #'f)
                                     (with-refinements?)
                                     (has-linear-integer-refinements? #'f))
                                tc-dep-fun-arg
                                single-value))
          ;; if for a particular argument, all of the domain types
          ;; agree for each arrow type in the case->, then we use
          ;; that type to check the argument expression against
          (define arg-types
            (for/list ([arg-stx (in-list args*)]
                       [arg-idx (in-naturals)])
              (define dom-ty (dom+rst-ref (car doms) (car rsts) arg-idx))
              (cond
                [(for/and ([dom (in-list (cdr doms))]
                           [rst (in-list (cdr rsts))])
                   (equal? dom-ty (dom+rst-ref dom rst arg-idx)))
                 (check-arg arg-stx (ret dom-ty))]
                [else (check-arg arg-stx)])))
          (tc/funapp #'f #'args f-ty arg-types expected)]
         [_ #:when (and (identifier? #'f)
                        (with-refinements?)
                        (has-linear-integer-refinements? #'f))
            (tc/funapp #'f #'args f-ty (map tc-dep-fun-arg args*) expected)]
         [_ (tc/funapp #'f #'args f-ty (map single-value args*) expected)]))]))

