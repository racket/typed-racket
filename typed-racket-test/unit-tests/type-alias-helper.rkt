#lang racket/base

;; Unit tests for the type-alias-helper.rkt module

(require "test-utils.rkt"
         racket/set
         rackunit
         syntax/id-table
         typed-racket/env/type-alias-helper)

(provide tests)
(gen-test-main)

;; two aliases in their own components
(define example-1
  (list (list #'x #'x)
        (list #'y #'y)))
;; all one component
(define example-2
  (list (list #'x #'x #'y)
        (list #'y #'x)))
;; two components, one with two nodes
(define example-3
  (list (list #'x #'y)
        (list #'y #'x)
        (list #'z)))
;; one with cycles, two that form a line
(define example-4
  (list (list #'x #'y)
        (list #'y #'x)
        (list #'a #'b)
        (list #'b)))
;; two large cycles
(define example-5
  (list (list #'x #'y #'z)
        (list #'y #'x)
        (list #'z #'x #'y)
        (list #'a #'b)
        (list #'b #'c)
        (list #'c #'a)))
;; check topological order
(define example-6
  (list (list #'a #'b)
        (list #'d)
        (list #'c #'d #'e)
        (list #'b #'c)
        (list #'e #'f)
        (list #'f)))

;; helper function for the tests below
;; ignores order of ids in the components and the
;; order of the components (because neither are stable
;; except for topological ordering).
(define (equal-id-sets? x y)
  (define (id-lsts->id-sets id-lsts)
    (for/set ([id-lst id-lsts])
      (for/set ([id id-lst]) (syntax-e id))))
  (equal? (id-lsts->id-sets x)
          (id-lsts->id-sets y)))

(define-binary-check (check-equal?/id equal-id-sets? actual expected))

(define tests
 (test-suite
  "Tests for type-alias-helper"
  (check-equal?/id (find-strongly-connected-type-aliases example-1)
                   (list (list #'x) (list #'y)))
  (check-equal?/id (find-strongly-connected-type-aliases example-2)
                   (list (list #'x #'y)))
  (check-equal?/id (find-strongly-connected-type-aliases example-3)
                   (list (list #'z) (list #'x #'y)))
  (check-equal?/id (find-strongly-connected-type-aliases example-4)
                   (list (list #'a) (list #'b) (list #'y #'x)))
  (check-equal?/id (find-strongly-connected-type-aliases example-5)
                   (list (list #'b #'a #'c) (list #'z #'x #'y)))
  (check-equal?/id (find-strongly-connected-type-aliases example-6)
                   (list (list #'a) (list #'b) (list #'c)
                         (list #'e) (list #'f) (list #'d)))))
