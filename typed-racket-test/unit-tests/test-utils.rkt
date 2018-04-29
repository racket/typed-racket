#lang racket/base

(require racket/require-syntax
         racket/match
         racket/gui/dynamic
         typed-racket/utils/utils
         (for-syntax racket/base syntax/parse)
         typed-racket/types/utils
         typed-racket/types/subtype
         typed-racket/utils/tc-utils
         typed-racket/typecheck/check-below
         typed-racket/rep/type-rep
         rackunit rackunit/text-ui)

(provide (all-defined-out))


;; checks for equality, modulo #f for prop or obj in
;; the expected (those are assumed to not matter if
;; #f, and so they are made equal to the given actual)
(define (tc-results-compat/test? actual expected)
  (define (below? res1 res2)
    (parameterize ([delay-errors? #f])
      (with-handlers ([exn:fail? (λ (_) #f)])
        (check-below res1 res2)
        #t)))

  (define expected*
    (match* (actual expected)
      [((tc-any-results: p1) (tc-any-results: p2))
       (-tc-any-results (or p2 p1))]
      [((tc-results: tcrs1 _) (tc-results: tcrs2 drst2))
       (-tc-results (for/list ([tcr1 (in-list tcrs1)]
                               [tcr2 (in-list tcrs2)])
                      (match* (tcr1 tcr2)
                        [((tc-result: _  ps1 o1)
                          (tc-result: t2 ps2 o2))
                         (-tc-result t2 (or ps2 ps1) (or o2 o1))]))
                    drst2)]
      [(_ _) expected]))

  (and (below? actual expected*)
       (below? expected* actual)))

(define-syntax gen-test-main
  (syntax-parser
    [(stx:id)
     #`(begin
         (module* main #f
           (require rackunit/text-ui)
           (void (run-tests #,(datum->syntax #'stx 'tests))))
         (module* test #f
           (require (submod ".." main))))]))
