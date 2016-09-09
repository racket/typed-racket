#lang racket/unit
(require "../utils/utils.rkt"
         (rep core-rep prop-rep)
         (types abbrev utils prop-ops)
         (utils tc-utils)
         (typecheck signatures tc-envops tc-metafunctions)
         (types type-table)
         (private syntax-properties)
         racket/match
         syntax/parse)

;; if typechecking
(import tc-expr^)
(export tc-if^)

(define (tc/if-twoarm tst thn els [expected #f])
  (match (single-value tst)
    [(tc-result1: _ (PropSet: ps+ ps-) _)
     (define expected* (and expected (erase-props expected)))
     (define results-t
       (with-lexical-env/extend-props (list ps+)
         #:unreachable (begin
                         (handle-unreachable-casted-exprs thn)
                         (warn-unreachable thn))
         (test-position-add-true tst)
         (tc-expr/check thn expected*)))
     (define results-u
       (with-lexical-env/extend-props (list ps-)
         #:unreachable (begin
                         (handle-unreachable-casted-exprs els)
                         (warn-unreachable els))
         (test-position-add-false tst)
         (tc-expr/check els expected*)))

     (merge-tc-results (list results-t results-u))]))

;; handle-unreachable-casted-exprs : Any -> Void
;; Traverses stx looking for casted-expr properties. For each one, it
;; calls the function stored in the property, which fills an entry in
;; the cast table with the -Dead-Code type. This is so that the
;; contract-generation pass doesn't throw an internal error.
(define (handle-unreachable-casted-exprs stx)
  (syntax-parse stx
    [(exp:casted-expr^ e)
     ;; fill in this entry in the cast table with the -Dead-Code type
     ((attribute exp.value) -Dead-Code)
     (void)]
    [stx
     (define e (syntax-e #'stx))
     (cond
       [(pair? e) (handle-unreachable-casted-exprs (car e))
                  (handle-unreachable-casted-exprs (cdr e))]
       [(box? e) (handle-unreachable-casted-exprs (unbox e))]
       [(vector? e) (for ([e (in-vector e)])
                      (handle-unreachable-casted-exprs e))]
       [(hash? e) (for ([(k v) (in-hash e)])
                    (handle-unreachable-casted-exprs k)
                    (handle-unreachable-casted-exprs v))]
       [(struct? e) (for ([e (in-vector (struct->vector e))])
                      (handle-unreachable-casted-exprs e))]
       [else (void)])]))

