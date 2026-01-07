#lang typed/racket

;; Test for numeric literal type checking against union types
;; Bug: numeric literals should be typed as Value types when the
;; expected type is a union of exact numeric values

(define-type Oops (U 1 2))

(: f (Oops -> Oops))
(define (f x) x)

;; Both of these should work
(f 1)
(f 2)

;; Test with larger numeric literals
(define-type Numbers (U 0 1 2 3 10 100))

(: g (Numbers -> Numbers))
(define (g x) x)

(g 0)
(g 1)
(g 2)
(g 3)
(g 10)
(g 100)

;; Test with negative numbers
(define-type SignedNumbers (U -1 0 1))

(: h (SignedNumbers -> SignedNumbers))
(define (h x) x)

(h -1)
(h 0)
(h 1)
