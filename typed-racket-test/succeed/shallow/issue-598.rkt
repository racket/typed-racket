#lang typed/racket/base/shallow

(module u racket/base
  (define (f b)
    (set-box! b "hello"))
  (provide f))

(define-type Maybe-Box (U #f (Boxof Integer)))

(require/typed 'u (f (-> Maybe-Box Void)))

(define b : Maybe-Box (box 4))

(module+ test
  (require typed/rackunit)

  (check-not-exn
    (λ () (f b)))

  (check-exn exn:fail:contract?
    (lambda () (if (box? b) (unbox b) (error 'deadcode)))))
