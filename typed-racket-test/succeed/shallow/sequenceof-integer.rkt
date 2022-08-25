#lang racket/base

(module typed typed/racket/base/shallow
  (provide foo)
  (: foo (-> (U Integer (Sequenceof Integer)) String))
  (define (foo x)
    (if (integer? x)
        (format "I got an integer: ~a" x)
        (error "I did not get an integer: ~a" x))))

(module other-typed typed/racket/base/shallow
  (provide bar)
  (require (submod ".." typed))
  (define (bar) (foo 0)))

(module contract-test typed/racket/base/shallow
  (define b* : (Sequenceof (Boxof Integer)) (list (box 0)))
  (provide b*))

(require 'typed
         'other-typed
         'contract-test
         rackunit)
(check-equal? (foo 0) "I got an integer: 0")
(check-equal? (bar) "I got an integer: 0")
(check-not-exn
  (λ () (set-box! (car b*) 'NaN)))
(check-not-exn ;; untyped context
  (λ () (unbox (car b*))))
