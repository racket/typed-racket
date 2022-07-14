#lang racket

(module typed typed/racket/base

  (define-type (LV a) (foo a))
  (struct (A) foo ([a : (LV A)]))
  (provide make)
  (define (make [in : (LV (LV Natural))]) : Void
    (void)))

(require 'typed)
;; (make '((1 1) (2 2) (3 3) (4 4)))
