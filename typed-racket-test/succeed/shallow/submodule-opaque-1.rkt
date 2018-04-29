#lang typed/racket/shallow

;; Test opaque import that would break Any-wrap contracts

(module u racket/base
  (define (posn? x)
    (when (box? x)
      (set-box! x 'nan))
    #false)
  (provide posn?))

(require typed/rackunit)
(require/typed 'u
  (#:opaque Posn posn?))

(define b : (Boxof Real) (box 0))

(check-not-exn
  (lambda ()
    (posn? b)))

(check-exn #rx"shape-check"
  (lambda ()
    (+ 1 (unbox b))))

