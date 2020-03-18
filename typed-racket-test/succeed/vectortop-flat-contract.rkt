#lang typed/racket/base
(require typed/rackunit)

;; Test that VectorTop generates a flat contract in cast,
;;  and a chaperone when exported to untyped code

(define v : (Mutable-Vectorof String) (vector "A"))
(void (cast v VectorTop))

(module u racket/base
  (provide f)
  (define (f x)
    (vector-set! x 0 'bad)))
(require/typed 'u
  (f (-> VectorTop Void)))

(check-exn exn:fail:contract? (lambda () (f v)))
