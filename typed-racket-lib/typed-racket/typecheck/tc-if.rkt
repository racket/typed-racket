#lang racket/unit
(require "../utils/utils.rkt" racket/match
         (rep core-rep prop-rep)
         (types utils prop-ops)
         (utils tc-utils)
         (typecheck signatures tc-envops tc-metafunctions)
         (types type-table))

;; if typechecking
(import tc-expr^)
(export tc-if^)

(define (tc/if-twoarm tst thn els [expected #f])
  (match (single-value tst)
    [(tc-result1: _ (PropSet: ps+ ps-) _)
     (define expected* (and expected (erase-props expected)))
     (define results-t
       (with-lexical-env/extend-props (list ps+)
         #:unreachable (warn-unreachable thn)
         (test-position-add-true tst)
         (tc-expr/check thn expected*)))
     (define results-u
       (with-lexical-env/extend-props (list ps-)
         #:unreachable (warn-unreachable els)
         (test-position-add-false tst)
         (tc-expr/check els expected*)))

     (merge-tc-results (list results-t results-u))]))
