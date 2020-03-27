#lang racket/base

;; Tests for equality predicates on static contract values

(require rackunit
         typed-racket/static-contracts/combinators)

;; these should be equal for optimizations to kick in
(check-equal? (flat/sc #'(quote 3)) (flat/sc #'(quote 3)))
(check-equal? (flat/sc #'(lambda (x) (integer? x)))
              (flat/sc #'(lambda (x) (integer? x))))

(define (make-stx id)
  #`(lambda (x) (#,id x)))
(define foo-stx-1 (make-stx #'foo?))
(define foo-stx-2 (make-stx #'foo?))

(parameterize ([current-namespace (make-base-namespace)])
  (eval #'(struct foo (x)))
  (define foo-stx-3 (make-stx (expand #'foo?)))
  (define foo-stx-4 (make-stx (expand #'foo?)))

  (check-equal? (flat/sc foo-stx-1) (flat/sc foo-stx-2))
  (check-equal? (flat/sc foo-stx-3) (flat/sc foo-stx-4))

  ;; these shouldn't be equal because bindings are different
  (check-not-equal? (flat/sc foo-stx-1) (flat/sc foo-stx-3)))
