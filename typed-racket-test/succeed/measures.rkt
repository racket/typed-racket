#lang typed/racket
(define-base-measure-unit m)
(define-base-measure-unit s)
(define-measure-unit m/s (u* m (u^ s -1)))
(define-type Real-Meters (Measure Real m))
(define-type Real-Seconds (Measure Real s))
(define-type Real-Meters/Second (Measure Real m/s))
(define x (measure 5 m))
(define t (measure 2 s))
(: f : Real-Meters Real-Seconds -> Real-Meters/Second)
(define (f x t)
  ; testing that (Measure Real (u* m (u^ s -1))) is the same as Real-Meters/Second
  (measure (/ x t) (u* m (u^ s -1))))
(f x t)
