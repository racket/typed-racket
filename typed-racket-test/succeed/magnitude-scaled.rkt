#lang typed/racket/base

(require typed/rackunit)

;; Ordinary values use direct squaring; extreme values need the scaled path.
(check-equal? (magnitude 3.0+4.0i) 5.0)
(check-equal? (magnitude 3e-200+4e-200i) 5e-200)
(check-= (magnitude 3e200+4e200i) 5e200 1e186)
(check-equal? (magnitude 1e308+1e308i) 1.4142135623730951e308)
