#lang racket/unit
(require "../utils/utils.rkt" racket/match
         "../rep/core-rep.rkt"
         "../rep/prop-rep.rkt"
         "../types/utils.rkt"
         "../types/prop-ops.rkt"
         "../utils/tc-utils.rkt"
         "signatures.rkt"
         "tc-envops.rkt"
         "tc-metafunctions.rkt"
         "../types/type-table.rkt")

;; if typechecking
(import tc-expr^)
(export tc-if^)

(define (tc/if-twoarm tst thn els [expected #f])
  (match (single-value tst)
    [(tc-result1: _ (PropSet: p+ p-) _)
     (define thn-res
       (with-lexical-env+props (list p+)
         #:expected expected
         #:unreachable (warn-unreachable thn)
         (test-position-add-true tst)
         (tc-expr/check thn expected)))
     (define els-res
       (with-lexical-env+props (list p-)
         #:expected expected
         #:unreachable (warn-unreachable els)
         (test-position-add-false tst)
         (tc-expr/check els expected)))

     (match expected
       ;; if there was not any expected results, then merge the 'then'
       ;; and 'else' results so we propogate the correct info upwards
       [(or #f (tc-any-results: #f))
        (merge-tc-results (list thn-res els-res))]
       ;; otherwise, the subcomponents have already been checked and
       ;; we just return the expected result 'fixed' to replace any
       ;; missing fields (i.e. #f props or objects)
       [_ (fix-results expected)])]))
