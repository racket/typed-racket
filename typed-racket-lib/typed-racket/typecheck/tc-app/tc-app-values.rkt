#lang racket/unit

(require "../../utils/utils.rkt"
         "signatures.rkt"
         "utils.rkt"
         syntax/parse racket/match racket/sequence
         (typecheck signatures tc-funapp tc-metafunctions)
         (types base-abbrev abbrev type-table utils)
         (rep values-rep)

         (for-label racket/base))


(import tc-expr^ tc-app^)
(export tc-app-values^)

(define-literal-set values-literals #:for-label (values call-with-values))

(define-tc/app-syntax-class (tc/app-values expected)
  #:literal-sets (values-literals)
  ;; call-with-values
  (pattern ((~and op-name call-with-values) prod con)
    #:do [(define prod-ty (tc-expr/t #'prod))]
    (match (tc/funapp #'prod #'() prod-ty null #f)
      [(tc-results: tcrs #f)
       (define con-ty (tc-expr/t #'con))
       (define r
         (tc/funapp #'con #'(prod) con-ty
                    (for/list ([tcr (in-list tcrs)])
                      (-tc-results (list tcr) #f))
                    expected))
       (define return-ty (tc-results->values r))
       (add-typeof-expr #'op-name (ret (-> prod-ty con-ty return-ty)))
       r]
      [(tc-results: _ (? RestDots?))
       (tc-error/expr "`call-with-values` with ... is not supported")]
      [(tc-any-results: _)
       (tc/app-regular this-syntax expected)]))
  ;; special case for `values' with single argument
  ;; we just ignore the values, except that it forces arg to return one value
  (pattern ((~and op-name values) arg)
    (let ([tc-result
           (match expected
            [(or #f (tc-any-results: _))
             (single-value #'arg)]
            [(tc-result1: tp)
             (single-value #'arg expected)]
             ;; Type check the argument, to find other errors
            [_ (single-value #'arg)])])
      (define arg-ty
        ;; match never fails; `single-value` always returns a tc-result1
        (match tc-result [(tc-result1: t) t]))
      (add-typeof-expr #'op-name (ret (-> arg-ty arg-ty)))
      tc-result))
  ;; handle `values' specially
  (pattern ((~and op-name values) . args)
    (match expected
      [(or (tc-results: tcrs #f)
           (bind tcrs '()))
       (define res
         (-tc-results
           (for/list ([arg (in-syntax #'args)]
                      [tcr (in-list/rest tcrs #f)])
             (match (single-value arg (and tcr (-tc-results (list tcr) #f)))
               [(tc-results: (list res) #f) res])) #f))
       (define return-ty (tc-results->values res))
       (define arg-tys (match return-ty [(Values: (list (Result: t* _ _) ...)) t*]))
       (add-typeof-expr #'op-name (ret (->* arg-tys return-ty)))
       res])))
