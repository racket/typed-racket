#lang racket/base

(module t typed/racket/optional
  (: f (case->))
  (define (f x) 0)
  (provide f))

(require 't rackunit)
(check-not-exn (lambda () (f (box 0))))

