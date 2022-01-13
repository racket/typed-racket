#lang racket/base

(require "tr-random-testing.rkt" racket/runtime-path racket/file racket/list)

;; list of all the counterexamples that the random tester found on drdr,
;; as of drdr run #32529
;; we test that there has been no regression, and print any that are still bugs
(define-runtime-path counterexamples.rktd "counterexamples.rktd")

(define counterexamples
  (parameterize ([read-single-flonum (single-flonum-available?)])
    (remove-duplicates (file->list counterexamples.rktd))))

(require racket/pretty)
(parameterize ([current-output-port (current-error-port)])
  (for ([c (in-list counterexamples)])
    (define-values (res cpu r g) (time-apply check-all-reals (list c)))
    (when (> cpu 1000)
      (log-info "slow expression: ~s ~s ~s\n" cpu g c))
    (unless (apply values res)
      (displayln c)
      (newline))))
