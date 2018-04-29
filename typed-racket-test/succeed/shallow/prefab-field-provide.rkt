#lang racket/base

(module m typed/racket/base/shallow

  (struct foo ([x : Integer]) #:prefab)
  (define f (foo 42))
  (struct bar ([y : Integer]) #:prefab #:mutable)
  (define b (bar 43))
  (provide f foo-x b bar-y set-bar-y!))

(module n racket/base
  (require (submod ".." m) rackunit)
  (set-bar-y! b 44)
  (check-not-exn (lambda () (set-bar-y! b "44"))))

(require (submod 'n))
