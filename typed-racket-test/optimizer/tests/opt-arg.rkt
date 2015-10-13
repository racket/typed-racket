#lang typed/racket/base

;; The optimizer now looks at all expressions regardless of shape (to log
;; all expressions with type (Vectorof Float) as candidates for uses of
;; flvectors).
;; This revealed that, previously, the optimizer did not skip the generated
;; body of optional arg function definitions (which is not typechecked, and so
;; should be skipped). It did for kw arg functions, but not optional.
;; This is now fixed, and this test is to guard against regressions.

(define (slicef-at [force? #f])
  #f)
