#lang racket/base

(require racket/lazy-require)

(lazy-require
 ("../typecheck/tc-envops.rkt" (implies-in-env?))
 ("../typecheck/tc-subst.rkt" (instantiate-rep/obj)))

(provide (rename-out [implies-in-env?-for-stupid-infer-unit
                      implies-in-env?]))

;; the units for infer currently make it impossible to do
;; lazy requires
(define (implies-in-env?-for-stupid-infer-unit env p q)
  (implies-in-env? env p q))