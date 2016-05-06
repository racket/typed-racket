#lang racket/unit

(require "../../utils/utils.rkt"
         "signatures.rkt"
         "utils.rkt"
         syntax/parse racket/match racket/sequence
         (typecheck signatures tc-funapp)
         (types base-abbrev utils)

         (for-label racket/base))


(import tc-expr^ tc-app^)
(export tc-app-values^)

(define-literal-set values-literals #:for-label (values call-with-values))

(define-tc/app-syntax-class (tc/app-values expected)
  #:literal-sets (values-literals)
  ;; call-with-values
  (pattern (call-with-values prod con)
    (match (tc/funapp #'prod #'() (tc-expr/t #'prod) null #f)
      [(tc-results: ts fs os)
       (tc/funapp #'con #'(prod) (tc-expr/t #'con) (map ret ts fs os) expected)]
      [(tc-results: ts fs os drest dbound)
       (tc-error/expr "`call-with-values` with ... is not supported")]
      [(tc-any-results: _)
       (tc/app-regular this-syntax expected)]))
  ;; special case for `values' with single argument
  ;; we just ignore the values, except that it forces arg to return one value
  (pattern (values arg)
    (match expected
     [(or #f (tc-any-results: _)) (single-value #'arg)]
     [(tc-result1: tp)
      (single-value #'arg expected)]
     [(tc-results: ts)
      (single-value #'arg)] ;Type check the argument, to find other errors
     ;; match polydots case and error
     [(tc-results: ts _ _ dty dbound)
      (single-value #'arg)]))
  ;; handle `values' specially
  (pattern (values . args)
    (match expected
      [(tc-results: ets efs eos)
       (match-let ([(list (tc-result1: ts fs os) ...)
                    (for/list
                      ([arg (in-syntax #'args)]
                       [et (in-sequences (in-list ets) (in-cycle (in-value #f)))]
                       [ef (in-sequences (in-list efs) (in-cycle (in-value #f)))]
                       [eo (in-sequences (in-list eos) (in-cycle (in-value #f)))])
                      (if et
                          (single-value arg (ret et ef eo))
                          (single-value arg)))])
         (ret ts fs os))]
      [_ (match-let ([(list (tc-result1: ts fs os) ...)
                      (for/list ([arg (in-syntax #'args)])
                        (single-value arg))])
           (ret ts fs os))])))
