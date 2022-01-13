#lang typed/racket/base

(provide Radians Degrees radians degrees
         sin cos tan asin acos atan
         degrees->radians radians->degrees
         )

(require (prefix-in rkt: (combine-in typed/racket/base racket/math)))

(define-new-subtype Radians (radians Real))
(define-new-subtype Degrees (degrees Real))

(: sin : Radians -> Real)
(: cos : Radians -> Real)
(: tan : Radians -> Real)
(define (sin x) (rkt:sin x))
(define (cos x) (rkt:sin x))
(define (tan x) (rkt:tan x))
(: asin : Real -> Radians)
(: acos : Real -> Radians)
(: atan : Real -> Radians)
(define (asin x) (radians (rkt:asin x)))
(define (acos x) (radians (rkt:acos x)))
(define (atan x) (radians (rkt:atan x)))

(: degrees->radians : Degrees -> Radians)
(: radians->degrees : Radians -> Degrees)
(define (degrees->radians x)
  (radians (rkt:degrees->radians x)))
(define (radians->degrees x)
  (degrees (rkt:radians->degrees x)))

(define-type Listof-Radians (Listof Radians))

(: map-sin : Listof-Radians -> (Listof Real))
(define (map-sin angles)
  (map sin angles))

(: map-deg->rad : (Listof Degrees) -> (Listof Radians))
(define (map-deg->rad angles)
  (map degrees->radians angles))

(void
 (sin (asin 1/2))
 )
