#lang racket/base

(require
 rackunit
 rackunit/text-ui
 racket/match racket/place
 rackunit/private/format
 (for-syntax racket/base syntax/parse racket/syntax))

(provide run-unit-test-suite)

(define-syntax define-tests
  (syntax-parser
    [(_ test-name:id files:expr ...)
     (define/with-syntax (new-names ...)
       (generate-temporaries #'(files ...)))
     #'(begin
         (require (only-in files [tests new-names]) ...)
         (define test-name
           (list new-names ...)))]))

(define-tests unit-tests
  "typecheck-tests.rkt"
  "subtype-tests.rkt"
  "type-equal-tests.rkt"
  "remove-intersect-tests.rkt"
  "static-contract-conversion-tests.rkt"
  "static-contract-instantiate-tests.rkt"
  "static-contract-optimizer-tests.rkt"
  "parse-type-tests.rkt"
  "subst-tests.rkt"
  "infer-tests.rkt"
  "keyword-expansion-test.rkt"
  "special-env-typecheck-tests.rkt"
  "contract-tests.rkt"
  "interactive-tests.rkt"
  "type-printer-tests.rkt"
  "type-alias-helper.rkt"
  "class-tests.rkt"
  "class-util-tests.rkt"
  "check-below-tests.rkt"
  "init-env-tests.rkt"
  "prop-tests.rkt"
  "metafunction-tests.rkt"
  "generalize-tests.rkt"
  "prims-tests.rkt"
  "tooltip-tests.rkt"
  "prefab-tests.rkt"
  "json-tests.rkt"
  "typed-units-tests.rkt"
  "type-constr-tests.rkt")

(struct fold-result [success failure err names])

;; This parallel test runner is somewhat unrelated to typed racket and it treats
;; a test suite as the smallest unit, which is not ideal at all, but it gives us
;; a performance improvement and its implementation is
;; straightforward. Ultimately, we can reuse parts or even all of `start-worker`
;; and `run` below to parallize rackunit after instances of test-case and
;; test-suite become place-message-allowable
(define (start-worker get-ch)
  (place/context aa
    (let loop ([st (fold-result 0 0 0 (list))])
      ;; we have to the index for a test suite instead of itself, because a test
      ;; suite is not place-message-allowed?
      [define idx (place-channel-get get-ch)]
      (cond
        [(equal? idx 'over)
         (match-define (struct fold-result (success failure err _)) st)
         (place-channel-put aa (list success failure err))]
        [else
         (loop
          (fold-test-results
           (lambda (r st)
             (display-test-result r #:suite-names (fold-result-names st))
             (cond
               [(test-success? r)
                (struct-copy fold-result st [success (add1 (fold-result-success st))])]
               [(test-failure? r)
                (struct-copy fold-result st [failure (add1 (fold-result-failure st))])]
               [else (struct-copy fold-result st [err (add1 (fold-result-err st))])]))
           st
           (list-ref unit-tests idx)
           #:fdown (lambda (name st)
                     (struct-copy fold-result st [names (cons name (fold-result-names st))]))
           #:fup (lambda (name st)
                   (struct-copy fold-result st [names (cdr (fold-result-names st))]))))]))))

(define (run n-workers)
  (define-values (put-ch get-ch) (place-channel))
  (define workers (build-list n-workers (lambda (id) (start-worker get-ch))))
  (for/list ([(_ i) (in-indexed unit-tests)])
    (place-channel-put put-ch i))
  (for ([w (in-list workers)])
    (place-channel-put put-ch 'over))

  (for/fold ([success 0]
             [failure 0]
             [err 0]
             #:result (begin
                        (printf "~a success(es) ~a failure(s) ~a error(s) ~a test(s) run ~n"
                                success failure err (+ success failure err))
                        (if (and (= failure 0) (= err 0)) 0
                            1)))
            ([w (in-list workers)])
    (match-define (list s f e) (place-channel-get w))
    (values (+ s success)
            (+ f failure)
            (+ e err))))

(define OPTIMIZED-N-WORKERS 4)

(define (run-unit-test-suite n-workers)
  (if (= n-workers 1)
      (run-tests (make-test-suite "Unit Tests" unit-tests))
      (run (if (> n-workers OPTIMIZED-N-WORKERS)
               OPTIMIZED-N-WORKERS
               n-workers))))
(module+ main
  (run 4))
