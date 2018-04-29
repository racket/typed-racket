#lang racket/base

(module t typed/racket/shallow
  (: f (case->))
  (define (f x) 0)
  (provide f))

(require 't rackunit)
(check-exn exn:fail:contract? (lambda () (f (box 0))))

