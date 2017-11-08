#lang racket/base

(module typed typed/racket/base
  (provide foo)
  (: foo (-> (U Integer (Sequenceof Integer)) String))
  (define (foo x)
    (if (integer? x)
        (format "I got an integer: ~a" x)
        (error "I did not get an integer: ~a" x))))

(module other-typed typed/racket/base
  (provide bar)
  (require (submod ".." typed))
  (define (bar) (foo 0)))

(require 'typed
         'other-typed)
(foo 0)
(bar)
