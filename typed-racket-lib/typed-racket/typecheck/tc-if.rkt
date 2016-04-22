#lang racket/unit
(require "../utils/utils.rkt"
         (rep prop-rep)
         (types utils prop-ops)
         (utils tc-utils)
         (typecheck signatures tc-envops tc-metafunctions)
         (types type-table)
         racket/match)

;; if typechecking
(import tc-expr^)
(export tc-if^)

(define (tc/if-twoarm tst thn els [expected #f])
  (match (single-value tst)
    [(tc-result1: _ (PropSet: fs+ fs-) _)
     (define expected* (and expected (erase-props expected)))
     (define results-t
       (with-lexical-env/extend-props (list fs+)
         #:unreachable (warn-unreachable thn)
         (test-position-add-true tst)
         (tc-expr/check thn expected*)))
     (define results-u
       (with-lexical-env/extend-props (list fs-)
         #:unreachable (warn-unreachable els)
         (test-position-add-false tst)
         (tc-expr/check els expected*)))

     (merge-tc-results (list results-t results-u))]))
