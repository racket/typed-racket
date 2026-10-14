#;
(exn-pred #rx"vector-set!")
#lang typed/racket

;; Like erase-box-invariant-prop.rkt, but x is substituted away at an
;; application rather than erased by let. The mutable half of the
;; Vectorof type must widen to Mutable-VectorTop.

(define p
  ((lambda ([x : Any])
     (define v : (Vectorof (Any -> Boolean : #:+ (Number @ x)))
       (vector (lambda ([w : Any]) (number? x))))
     (cons v (lambda () (if ((vector-ref v 0) 0) (add1 x) 1))))
   (ann "" Any)))

(vector-set! (car p) 0 (lambda ([w : Any]) #t))
((cdr p))
