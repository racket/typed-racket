#lang racket/unit

(require "../../utils/utils.rkt"
         "signatures.rkt"
         "utils.rkt"
         syntax/parse racket/match 
         syntax/parse/experimental/reflect
         "../signatures.rkt" "../tc-funapp.rkt"
         (types utils)
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
(define (arr-matches? arr args)
  (match arr
    [(arr: domain
           (Values: (list (Result: v
                                   (PropSet: (TrueProp:) (TrueProp:))
                                   (Empty:)) ...))
           rest #f (list (Keyword: _ _ #f) ...))
     (cond
       [(< (length domain) (length args)) rest]
       [(= (length domain) (length args)) #t]
       [else #f])]
    [_ #f]))

(define (has-props? arr)
  (match arr
    [(arr: _ (Values: (list (Result: v
                                     (PropSet: (TrueProp:) (TrueProp:))
                                     (Empty:)) ...))
           _ _ (list (Keyword: _ _ #f) ...)) #f]
    [else #t]))


(define (tc/app-regular form expected)
  (syntax-case form ()
    [(f . args)
     (let* ([f-ty (tc-expr/t #'f)]
            [args* (syntax->list #'args)])
       (define (matching-arities arrs)
         (for/list ([arr (in-list arrs)] #:when (arr-matches? arr args*)) arr))
       (define (has-drest/props? arrs)
         (for/or ([arr (in-list arrs)])
           (or (has-props? arr) (arr-drest arr))))

       (define arg-tys
         (match f-ty
           [(Function: (? has-drest/props?))
            (map single-value args*)]
           [(Function:
              (app matching-arities
                (list (arr: doms ranges rests drests _) ..1)))
            (define matching-domains
              (in-values-sequence
                (apply in-parallel
                  (for/list ((dom (in-list doms)) (rest (in-list rests)))
                    (in-sequences (in-list dom) (in-cycle (in-value rest)))))))
            (for/list ([a (in-list args*)] [types matching-domains])
              (match-define (cons t ts) types)
              (if (for/and ((t2 (in-list ts))) (equal? t t2))
                  (tc-expr/check a (ret t))
                  (single-value a)))]
           [_ (map single-value args*)]))
       (tc/funapp #'f #'args f-ty arg-tys expected))]))
