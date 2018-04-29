#lang typed/racket/base/optional

;; Make sure defender runs in module*

(module u racket/base
  (define b
    (box "hello"))
  (provide b))

(require/typed 'u (b (Boxof Integer)))

(module* test #f
  (require typed/rackunit)

  (check-not-exn
    (lambda () (unbox b))))
