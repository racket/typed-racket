#lang typed/racket

(require racket/flonum)

(define-for-syntax (->fl v) (datum->syntax #'here (if (single-flonum-available?) (real->single-flonum v) v)))

(define-syntax (fl stx)
  (syntax-case stx ()
    [(_ v) (->fl (syntax-e #'v))]))

(define-syntax (ann* stx)
  (if (eq? 'chez-scheme (system-type 'vm))
      (syntax-case stx ()
        [(_ e t) #'e])
      (syntax-case stx ()
        [(_ e t) #'(ann e t)])))
        

(ann* (fl 3.0f0) Single-Flonum)

(ann (real->double-flonum (fl 0.0f0)) Flonum-Positive-Zero)
(ann (real->double-flonum (fl -0.0f0)) Flonum-Negative-Zero)
(when (single-flonum-available?)
  (ann  (/ (round (exact-round (fl -2.7393196f0))) (real->double-flonum (inexact->exact (real->single-flonum -0.0)))) Real)

  (ann (real->single-flonum (fl 0.0f0)) Single-Flonum-Positive-Zero)
  (ann (real->single-flonum (fl -0.0f0)) Single-Flonum-Negative-Zero))
(ann* (fl 34.2f0) Positive-Single-Flonum)
(ann* (fl -34.2f0) Negative-Single-Flonum)
(ann (expt (ann* (fl 0.5f0) Single-Flonum) (ann 2 Natural)) Real)
(ann (expt
      (sub1 (gcd (exact-round 1)))
      (- (ceiling (real->double-flonum (fl -2.6897657f0)))))
     Real)
(ann  (expt (make-rectangular 3 -1.7976931348623157e+308)
            (flacos (real->double-flonum (fl 59.316513f0))))
      (U Flonum Float-Complex))
(ann  (expt (fl 0.0f0) -3.0) Real)
(ann  (expt -8.665778974912815f+107 -677460115195106837726964554590085563061636191189747) Number)
(ann  (expt (sin +inf.f) +nan.0+nan.0i) Number)
(ann  (/ (gcd 1 0) 0.0f0 2.718281828459045) Real)
(ann  (expt (make-polar (floor 6.468476f+31) (tanh +nan.f)) +nan.0) Number)

