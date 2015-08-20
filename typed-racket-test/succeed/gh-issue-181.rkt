#lang typed/racket

;; Test case for GH issue #181

(module m1 typed/racket
  (provide bar)
  (define-syntax-rule (bar) 42))

(module m2 typed/racket
  (require typed/racket
           (submod ".." m1))
  (provide (all-from-out typed/racket)
           bar))

(module m3 (submod ".." m2)
  (bar))
