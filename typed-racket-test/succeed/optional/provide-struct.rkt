#lang racket/base

;; Test providing a struct (and its types)

(module t typed/racket/base/optional

  (provide (struct-out foo) wepa)

  (struct foo ((a : Natural)))

  (define (wepa (f : foo))
    (+ 1 (foo-a f))))

(require 't rackunit)

(check-pred values (foo 1))
(check-pred values (foo 'a))

(check-exn #rx"\\+: contract violation"
  (λ () (wepa (foo 'a))))

(check-equal?
  (foo-a (foo 'a))
  'a)
