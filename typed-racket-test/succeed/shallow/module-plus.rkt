#lang typed/racket/base/shallow

;; Make sure defender runs in module+

(module u racket/base
  (define b
    (box "hello"))
  (provide b))

(require/typed 'u (b (Boxof Integer)))

(module+ test
  (require typed/rackunit)

  (check-exn exn:fail:contract?
    (lambda () (unbox b))))
