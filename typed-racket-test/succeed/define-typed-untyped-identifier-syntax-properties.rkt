#lang racket

;; https://github.com/racket/typed-racket/issues/315

(module t/u racket
  (provide typed/untyped
           (rename-out [typed/untyped-typed original-typed-req-stx]
                       [typed/untyped-untyped original-untyped-req-stx]))
  
  (require typed/untyped-utils
           racket/require-syntax
           (for-syntax syntax/strip-context))
  
  (define-require-syntax (typed/untyped-typed stx)
    (syntax-case stx ()
      [(_ m s) (replace-context stx #'(submod m s typed))]))
  
  (define-require-syntax (typed/untyped-untyped stx)
    (syntax-case stx ()
      [(_ m s) (replace-context stx #'(submod m s untyped))]))
  
  (define-typed/untyped-identifier typed/untyped
    typed/untyped-typed
    typed/untyped-untyped))

(module m racket
  (module typed typed/racket
    (provide test-value typed-test-value)
    (define test-value : Symbol 'is-typed)
    (define typed-test-value : Symbol 'present))
  (module untyped typed/racket/no-check
    (provide test-value untyped-test-value)
    (define test-value : Symbol 'is-untyped)
    (define untyped-test-value : Symbol 'present)))

(module test-typed-1 typed/racket
  (require (submod ".." t/u))
  (require typed/rackunit)
  (require (original-typed-req-stx ".." m))
  (check-equal? test-value 'is-typed)
  (check-equal? typed-test-value 'present))

(module test-typed-2 typed/racket
  (require (submod ".." t/u))
  (require typed/rackunit)
  (require (typed/untyped ".." m))
  (check-equal? test-value 'is-typed)
  (check-equal? typed-test-value 'present))

(module test-typed-3 typed/racket
  (require (submod ".." t/u))
  (require typed/rackunit)
  (define-syntax renamer (make-rename-transformer #'original-typed-req-stx))
  (require (renamer ".." m))
  (check-equal? test-value 'is-typed)
  (check-equal? typed-test-value 'present))

(module test-untyped-1 racket
  (require (submod ".." t/u))
  (require rackunit)
  (require (original-untyped-req-stx ".." m))
  (check-equal? test-value 'is-untyped)
  (check-equal? untyped-test-value 'present))

(module test-untyped-2 racket
  (require (submod ".." t/u))
  (require rackunit)
  (require (typed/untyped ".." m))
  (check-equal? test-value 'is-untyped)
  (check-equal? untyped-test-value 'present))

(module test-untyped-3 racket
  (require (submod ".." t/u))
  (require rackunit)
  (define-syntax renamer (make-rename-transformer #'original-untyped-req-stx))
  (require (renamer ".." m))
  (check-equal? test-value 'is-untyped)
  (check-equal? untyped-test-value 'present))

(require 'test-typed-1 'test-typed-2 'test-typed-3
         'test-untyped-1 'test-untyped-2 'test-untyped-3)