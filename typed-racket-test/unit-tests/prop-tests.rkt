#lang racket/base

(require "test-utils.rkt"
         rackunit racket/format
         (rep prop-rep)
         (types abbrev union prop-ops)
         (for-syntax racket/base syntax/parse))

(provide tests)
(gen-test-main)

(define (not-implies-atomic? y x) (not (implies-atomic? y x)))

(define-syntax (test-opposite stx)
  (define-syntax-class complementary
     (pattern #:complementary #:with check #'check-true)
     (pattern #:not-complementary #:with check #'check-false))
  (define-syntax-class contradictory
     (pattern #:contradictory #:with check #'check-true)
     (pattern #:not-contradictory #:with check #'check-false))
  (syntax-parse stx
    [(_ comp:complementary contr:contradictory p1* p2*)
     (syntax/loc stx
       (test-case (~a '(opposite p1* p2*))
         (define p1 p1*)
         (define p2 p2*)
         (comp.check (complementary? p1 p2) "Complementary")
         (contr.check (contradictory? p1 p2) "Contradictory")))]))


(define tests
  (test-suite "Props"
    (test-suite "Opposite"
      (test-opposite #:not-complementary #:contradictory
        (-is-type 0 -Symbol)
        (-not-type 0 (Un -Symbol -String)))

      (test-opposite #:complementary #:not-contradictory
        (-is-type 0 (Un -Symbol -String))
        (-not-type 0 -Symbol))

      (test-opposite #:complementary #:contradictory
        (-not-type 0 -Symbol)
        (-is-type 0 -Symbol))

      (test-opposite #:not-complementary #:not-contradictory
        (-is-type 1 -Symbol)
        (-not-type 0 -Symbol))

      (test-opposite #:not-complementary #:not-contradictory
        (-not-type 0 -Symbol)
        (-is-type 0 -String))

      (test-opposite #:not-complementary #:not-contradictory
        (-not-type 0 -Symbol)
        (-is-type 0 -String))

      (test-opposite #:not-complementary #:contradictory
        -ff
        -ff)

      (test-opposite #:complementary #:contradictory
        -ff
        -tt)

      (test-opposite #:complementary #:not-contradictory
        -tt
        -tt)

    )

    (test-suite "Implies Atomic"
      (check implies-atomic?
             -tt -tt)
      (check implies-atomic?
             -ff -ff)
      (check implies-atomic?
             -ff -tt)
      (check not-implies-atomic?
             -tt -ff)
      (check implies-atomic?
             (-is-type 0 -Symbol) -tt)
      (check implies-atomic?
             -ff (-is-type 0 -Symbol))
      (check implies-atomic?
             (-is-type 0 -Symbol)
             (-is-type 0 (Un -String -Symbol)))
      (check not-implies-atomic?
             (-is-type 0 (Un -String -Symbol))
             (-is-type 0 -Symbol))
      (check implies-atomic?
             (-not-type 0 (Un -String -Symbol))
             (-not-type 0 -Symbol))
      (check not-implies-atomic?
             (-not-type 0 -Symbol)
             (-not-type 0 (Un -String -Symbol)))
      (check not-implies-atomic?
             (-is-type 0 -Symbol)
             (-is-type 1 -Symbol))
      (check implies-atomic?
             (-is-type #'x -Symbol)
             (-is-type #'x -Symbol))
      (check implies-atomic?
             (-is-type #'x -Symbol)
             (-or (-is-type 1 -Symbol) (-is-type #'x -Symbol)))
      (check implies-atomic?
             (-and (-is-type 1 -Symbol) (-is-type #'x -Symbol))
             (-is-type #'x -Symbol))
      (check implies-atomic?
             (-is-type #'x -Symbol)
             (-not-type #'x (-val #f)))
    )

    (test-suite "Simplification"
      (check-equal?
        (-and (-is-type #'x -Symbol) (-not-type #'x (-val #f)))
        (-is-type #'x -Symbol))
      (check-equal?
        (-and (-not-type #'x (-val #f)) (-is-type #'x -Symbol))
        (-is-type #'x -Symbol))

      (check-equal?
        (-and (-is-type #'y (-val #f))
              (-or (-is-type #'y (-val #f))
                   (-is-type #'x (-val #f))))
        (-is-type #'y (-val #f)))

      (check-equal?
        (-and (-not-type #'y (-val #f))
              (-or (-not-type #'y (-val #f))
                   (-not-type #'x (-val #f))))
        (-not-type #'y (-val #f))))

  ))
