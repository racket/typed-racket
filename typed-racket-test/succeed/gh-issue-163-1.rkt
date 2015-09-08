#lang typed/racket

;; Test for GH issue 163

(: bar (case→ (→ 'a True) (→ 'b False)))
(define (bar x) (if (eq? x 'a) #t #f))

(module m typed/racket
  (: foo (case→ (→ 'a True) (→ 'b False)))
  (define (foo x) (if (eq? x 'a) #t #f))

  (provide foo))

(require 'm)

(module+ test
  (define b bar)
  (define f foo))
