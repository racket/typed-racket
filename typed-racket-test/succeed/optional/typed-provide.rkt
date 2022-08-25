#lang racket/base

;; Test providing a value to an untyped context

(module a typed/racket/base
  (provide f)
  (define (f (x : (Boxof (Boxof Integer))))
    (unbox (unbox x))))

(module b racket/base
  (provide bbx)
  (define bbx (box 0)))

(module c typed/racket/base/optional
  (require (submod ".." a))
  (require/typed (submod ".." b) (bbx (Boxof (Boxof Integer))))
  (provide do-c bbx)
  (define (do-c)
    (f bbx)))

(require 'a rackunit)
(check-exn #rx"f: contract violation"
  (lambda () (f 0)))

(require 'c)
(check-not-exn (lambda () (unbox bbx)))
(check-exn #rx"f: contract violation"
  do-c)
