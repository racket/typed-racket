#lang racket/base

(require racket/require-syntax
         racket/match
         racket/gui/dynamic
         typed-racket/utils/utils
         (for-syntax racket/base syntax/parse)
         (types utils subtype)
         (utils tc-utils)
         (typecheck check-below)
         (rep type-rep)
         rackunit rackunit/text-ui)

(provide private typecheck (rename-out [infer r:infer]) utils env rep types base-env static-contracts
         (all-defined-out))

(define (tc-result-equal/test? res1 res2)
  (define (below? res1 res2)
    (parameterize ([delay-errors? #f])
      (with-handlers ([exn:fail? (λ (_) #f)])
        (check-below res1 res2)
        #t)))
  (and (below? res1 res2)
       (below? res2 res1)))

(define-syntax (check-type-equal? stx)
  (syntax-case stx ()
    [(_ nm a b)
     (syntax/loc stx (test-check nm type-equal? a b))]))

(define-syntax gen-test-main
  (syntax-parser
    [(stx:id)
     #`(begin
         (module* main #f
           (require rackunit/text-ui)
           (void (run-tests #,(datum->syntax #'stx 'tests))))
         (module* test #f
           (require (submod ".." main))))]))
