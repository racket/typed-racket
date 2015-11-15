#lang racket
(module m1 typed/racket

  (provide (for-meta 1 +)))

(module m2 racket/base
  (require (submod ".." m1))
  (begin-for-syntax +))