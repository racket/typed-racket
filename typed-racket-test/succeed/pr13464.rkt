#lang racket

(module defs typed/racket
  (provide (all-defined-out))

  (: neg-flonum     Negative-Flonum)
  (: pos-flonum     Positive-Flonum)
  (: non-neg-flonum Nonnegative-Flonum)
  (: non-pos-flonum Nonpositive-Flonum)


  (: neg-ineact-real     Negative-Inexact-Real)
  (: pos-ineact-real     Positive-Inexact-Real)
  (: non-neg-ineact-real Nonnegative-Inexact-Real)
  (: non-pos-ineact-real Nonpositive-Inexact-Real)

  (: neg-real     Negative-Real)
  (: pos-real     Positive-Real)
  (: non-neg-real Nonnegative-Real)
  (: non-pos-real Nonpositive-Real)

  (define neg-flonum     +nan.0)
  (define pos-flonum     +nan.0)
  (define non-neg-flonum +nan.0)
  (define non-pos-flonum +nan.0)

  (define neg-ineact-real     +nan.0)
  (define pos-ineact-real     +nan.0)
  (define non-neg-ineact-real +nan.0)
  (define non-pos-ineact-real +nan.0)

  (define neg-real     +nan.0)
  (define pos-real     +nan.0)
  (define non-neg-real +nan.0)
  (define non-pos-real +nan.0)
  
  ;; extra tests for zeroes
  (: non-neg-flonum+0 Nonnegative-Flonum)
  (: non-pos-flonum+0 Nonpositive-Flonum)
  (: non-neg-flonum-0 Nonnegative-Flonum)
  (: non-pos-flonum-0 Nonpositive-Flonum)


  (: non-neg-ineact-real+0 Nonnegative-Inexact-Real)
  (: non-pos-ineact-real+0 Nonpositive-Inexact-Real)
  (: non-neg-ineact-real-0 Nonnegative-Inexact-Real)
  (: non-pos-ineact-real-0 Nonpositive-Inexact-Real)

  (: non-neg-real+0 Nonnegative-Real)
  (: non-pos-real+0 Nonpositive-Real)
  (: non-neg-real-0 Nonnegative-Real)
  (: non-pos-real-0 Nonpositive-Real)

  (define non-neg-flonum+0 0.0)
  (define non-pos-flonum+0 0.0)
  (define non-neg-flonum-0 -0.0)
  (define non-pos-flonum-0 -0.0)

  (define non-neg-ineact-real+0 0.0)
  (define non-pos-ineact-real+0 0.0)
  (define non-neg-ineact-real-0 -0.0)
  (define non-pos-ineact-real-0 -0.0)

  (define non-neg-real+0 0.0)
  (define non-pos-real+0 0.0)
  (define non-neg-real-0 -0.0)
  (define non-pos-real-0 -0.0)

  )


(require 'defs)

neg-flonum
pos-flonum
non-neg-flonum
non-pos-flonum
neg-ineact-real
pos-ineact-real
non-neg-ineact-real
non-pos-ineact-real
neg-real
pos-real
non-neg-real
non-pos-real

non-neg-flonum+0
non-pos-flonum+0
non-neg-flonum-0
non-pos-flonum-0

non-neg-ineact-real+0
non-pos-ineact-real+0
non-neg-ineact-real-0
non-pos-ineact-real-0

non-neg-real+0
non-pos-real+0
non-neg-real-0
non-pos-real-0


#reader tests/racket/maybe-single
(module single-defs typed/racket
  (provide (all-defined-out))

  (define-syntax (:: stx)
    (syntax-case stx ()
      [(_ id t)
       (if (single-flonum-available?)
           #'(: id t)
           #'(: id Flonum))]))
  
  (:: neg-single-flonum     Negative-Single-Flonum)
  (:: pos-single-flonum     Positive-Single-Flonum)
  (:: non-neg-single-flonum Nonnegative-Single-Flonum)
  (:: non-pos-single-flonum Nonpositive-Single-Flonum)
  (define neg-single-flonum     +nan.f)
  (define pos-single-flonum     +nan.f)
  (define non-neg-single-flonum +nan.f)
  (define non-pos-single-flonum +nan.f)
  (:: non-neg-single-flonum+0 Nonnegative-Single-Flonum)
  (:: non-pos-single-flonum+0 Nonpositive-Single-Flonum)
  (:: non-neg-single-flonum-0 Nonnegative-Single-Flonum)
  (:: non-pos-single-flonum-0 Nonpositive-Single-Flonum)

  (define non-neg-single-flonum+0 0.0f0)
  (define non-pos-single-flonum+0 0.0f0)
  (define non-neg-single-flonum-0 -0.0f0)
  (define non-pos-single-flonum-0 -0.0f0)
)

(module single-use racket
  (require (submod ".." single-defs))
  neg-single-flonum
  pos-single-flonum
  non-neg-single-flonum
  non-pos-single-flonum

  non-neg-single-flonum+0
  non-pos-single-flonum+0
  non-neg-single-flonum-0
  non-pos-single-flonum-0
  )

(module ext-defs typed/racket
  (provide (all-defined-out))

  (define-syntax (:: stx)
    (syntax-case stx ()
      [(_ id t)
       (if (single-flonum-available?)
           #'(: id t)
           #'(: id ExtFlonum))]))

  (:: neg-extflonum     Negative-ExtFlonum)
  (:: pos-extflonum     Positive-ExtFlonum)
  (:: non-neg-extflonum Nonnegative-ExtFlonum)
  (:: non-pos-extflonum Nonpositive-ExtFlonum)

  (define neg-extflonum     +nan.t)
  (define pos-extflonum     +nan.t)
  (define non-neg-extflonum +nan.t)
  (define non-pos-extflonum +nan.t)

  (:: non-neg-extflonum+0 Nonnegative-ExtFlonum)
  (:: non-pos-extflonum+0 Nonpositive-ExtFlonum)
  (:: non-neg-extflonum-0 Nonnegative-ExtFlonum)
  (:: non-pos-extflonum-0 Nonpositive-ExtFlonum)

  (define non-neg-extflonum+0 0.0t0)
  (define non-pos-extflonum+0 0.0t0)
  (define non-neg-extflonum-0 -0.0t0)
  (define non-pos-extflonum-0 -0.0t0))

(module ext-use racket
  (require (submod ".." ext-defs))
  neg-extflonum
  pos-extflonum
  non-neg-extflonum
  non-pos-extflonum

  non-neg-extflonum+0
  non-pos-extflonum+0
  non-neg-extflonum-0
  non-pos-extflonum-0)

(require (submod "." ext-use))
(require (submod "." single-use))

