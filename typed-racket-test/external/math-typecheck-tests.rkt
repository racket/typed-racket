#lang racket/base

(require "../unit-tests/test-utils.rkt"
         rackunit
         racket/math
         typed/racket/flonum
         typed/racket/extflonum
         "../unit-tests/typecheck-tests.rkt"
         (submod "../unit-tests/typecheck-tests.rkt" test-helpers)
         (except-in (combine-in typed-racket/base-env/extra-procs
                                typed-racket/base-env/prims
                                typed-racket/base-env/base-types
                                typed-racket/base-env/base-types-extra)
                    define lambda λ case-lambda for/set for*/set)
         (for-syntax
          typed-racket/rep/core-rep
          typed-racket/rep/type-rep
          typed-racket/rep/prop-rep
          typed-racket/rep/object-rep
          typed-racket/rep/values-rep
          typed-racket/base-env/base-structs
          typed-racket/types/numeric-tower
          typed-racket/types/prop-ops
          typed-racket/types/utils
          typed-racket/types/resolve
          (rename-in typed-racket/types/abbrev
                     [Un t:Un]
                     [-> t:->])))
(provide tests)
(gen-test-main)

(define tests
  (test-suite
   "unit tests"
   #reader typed-racket/typed-reader
   (test-suite
    "math unit tests"
    (tc-e (expt (sqrt (+)) (cosh (flcos (real->double-flonum 0))))
          -Real)
    (tc-e (expt
           (tan (real->double-flonum 6))
           (lcm (*) (exact-round -1.7976931348623153e+308) 6))
          -Real))
   (tc-e (tanh (ann 0 Nonnegative-Integer)) -NonNegReal)
   (tc-e (bitwise-and (exact-round 1.7976931348623157e+308) (exact-round -29)) -Int)
   (tc-e (sinh (ann 0 Nonpositive-Integer)) -NonPosReal)
   [tc-e (sinh (let-values (((x y) (quotient/remainder (exact-round 1) (sqr (exact-round 1)))))
                 y))
         -Zero]
   ;; pr615 : positive-integer?
   [tc-e/t (let: ([x : Exact-Rational 3/2])
             (if (positive-integer? x) x 0))
           -Nat]
   [tc-e/t (let: ([x : Flonum 1.0])
             (if (positive-integer? x) x 2.0))
           -PosFlonum]
   [tc-e/t (let: ([x : (Un Flonum Positive-Integer) 1.0])
             (if (not (positive-integer? x)) x 1.0))
           -Flonum]
   ;; pr615 : negative-integer?
   [tc-e/t (let: ([x : Exact-Rational -3/2])
             (if (negative-integer? x) x -5))
           -NegInt]
   [tc-e/t (let: ([x : Flonum 1.0])
             (if (negative-integer? x) x -2.0))
           -NegFlonum]
   [tc-e/t (let: ([x : (Un Flonum Negative-Integer) -1.0])
             (if (not (negative-integer? x)) x -1.0))
           -Flonum]
   ;; pr615 : nonpositive-integer?
   [tc-e/t (let: ([x : Exact-Rational -3/2])
             (if (nonpositive-integer? x) x 0))
           -NonPosInt]
   [tc-e/t (let: ([x : Flonum -1.0])
             (if (nonpositive-integer? x) x 0.0))
           -NonPosFlonum]
   [tc-e/t (let: ([x : (Un Flonum Negative-Integer) -1.0])
             (if (not (nonpositive-integer? x)) x -1.0))
           -Flonum]
   ;; pr615 : nonnegative-integer?
   [tc-e/t (let: ([x : Exact-Rational 3/2])
             (if (nonnegative-integer? x) x 0))
           -Nat]
   [tc-e/t (let: ([x : Flonum 1.0])
             (if (nonnegative-integer? x) x 2.0))
           -NonNegFlonum]
   [tc-e/t (let: ([x : (Un Flonum Natural) 1.0])
             (if (not (nonnegative-integer? x)) x 1.0))
           -Flonum]
   ;; pr615 : natural?
   [tc-e/t (let: ([x : Real 1])
             (if (natural? x) x 1))
           -Nat]
   [tc-e/t (let: ([x : (Un Flonum Natural) 0.0])
             (if (not (natural? x)) x 1.0))
           -Flonum]))
