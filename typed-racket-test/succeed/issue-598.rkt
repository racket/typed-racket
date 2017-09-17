#lang typed/racket/base

(module u racket/base
  (define (f b)
    (set-box! b "hello"))
  (provide f))

(define-type Maybe-Box (U #f (Boxof Integer)))

(require/typed 'u (f (-> Maybe-Box Void)))

(define b : Maybe-Box (box 4))

(module+ test
  (require typed/rackunit)

  (check-exn exn:fail:contract?
    (λ () (f b)))

  (check-equal?
    (if (box? b) (+ 1 (unbox b)) (error 'deadcode))
    5))
