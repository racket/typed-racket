#lang typed/racket
(require racket/flonum
         typed/rackunit)


;;Flonum typechecking tests
;; -FlZero -Fl
(check-eq?
  (ann (let ([x : Flonum-Zero 0.0])
         (if (fl>= x (ann -4.0 Flonum))
           x -3.0))  Flonum)
  0.0)
(check-eq?
  (ann (let ([x : Flonum-Zero 0.0])
         (if (fl<= x (ann -4.0 Flonum))
           -3.0 x))  Flonum)
  0.0)

;; -PosFl -Fl
(check-eq?
  (ann (let ([x : Positive-Flonum 5.0])
         (if (fl>= x (ann 1.0 Flonum))
           x 1.0))  Flonum)
  5.0)
(check-eq?
  (ann (let ([x : Positive-Flonum 5.0])
         (if (fl<= x (ann 1.0 Flonum))
           1.0 x))  Flonum)
  5.0)

;; -NonNegFl -Fl
(check-eq?
  (ann
    (let ([x : Nonnegative-Flonum 5.0])
      (if (fl>= x (ann 1.0 Flonum))
        x 1.0))  Flonum)
  5.0)
(check-eq?
  (ann (let ([x : Nonnegative-Flonum 5.0])
         (if (fl<= x (ann 1.0 Flonum))
           1.0 x))  Flonum)
  5.0)

;; -NonPosFl -Fl
(check-eq?
  (ann (let ([x : Nonpositive-Flonum -1.0])
         (if (fl>= x (ann -5.0 Flonum))
           x -2.0)) Flonum)
  -1.0)
(check-eq?
  (ann (let ([x : Nonpositive-Flonum -1.0])
         (if (fl<= x (ann -5.0 Flonum))
           -2.0 x)) Flonum)
  -1.0)

;; -NegFl -Fl
(check-eq?
  (ann (let ([x : Negative-Flonum -1.0])
         (if (fl>= x (ann -5.0 Flonum))
           x -2.0)) Flonum)
  -1.0)
(check-eq?
  (ann (let ([x : Negative-Flonum -1.0])
         (if (fl<= x (ann -5.0 Flonum))
           -2.0 x)) Flonum)
  -1.0)


#|
;; These will all raise type errors.
;; -Fl -FlZero
(ann
  (let ([x : Flonum 5.0])
    (if (fl>= x (ann 0.0 Flonum-Zero))
      x 0.0))  Flonum-Zero)
(ann
  (let ([x : Flonum 5.0])
    (if (fl<= x (ann 0.0 Flonum-Zero))
      0.0 x))  Flonum-Zero)

;; -Fl -PosFl
(ann
  (let ([x : Flonum 5.0])
    (if (fl>= x (ann 1.0 Positive-Flonum))
      x 1.0))  Positive-Flonum)
(ann
  (let ([x : Flonum 5.0])
    (if (fl<= x (ann 1.0 Positive-Flonum))
      1.0 x))  Positive-Flonum)


;; -Fl -NonNegFl
(ann
  (let ([x : Flonum 5.0])
    (if (fl>= x (ann 1.0 Nonnegative-Flonum))
      x 1.0))  Nonnegative-Flonum)
(ann
  (let ([x : Flonum 5.0])
    (if (fl<= x (ann 1.0 Nonnegative-Flonum))
      1.0 x))  Nonnegative-Flonum)

;; -Fl -NegFl
(ann
  (let ([x : Flonum -1.0])
    (if (fl>= x (ann -5.0 Negative-Flonum))
      x -2.0)) Negative-Flonum)
(ann
  (let ([x : Flonum -1.0])
    (if (fl<= x (ann -5.0 Negative-Flonum))
      -2.0 x)) Negative-Flonum)

;; -Fl -NonPosFl
(ann
  (let ([x : Flonum 5.0])
    (if (fl>= x (ann -4.0 Nonpositive-Flonum))
      x -3.0))  Nonpositive-Flonum)
(ann
  (let ([x : Flonum 5.0])
    (if (fl<= x (ann -4.0 Nonpositive-Flonum))
      -3.0 x))  Nonpositive-Flonum)

|#

