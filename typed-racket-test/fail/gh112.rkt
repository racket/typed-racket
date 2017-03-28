#lang typed/racket

(ann
  (let ([x : Flonum -5.0])
    (if (< x (ann +nan.0 Positive-Flonum))
        1.0
        x))
  Positive-Flonum) ; -5 is *not* positive
