#lang racket/load

;; Test that variable redefinition works at the top-level

(require typed/racket/optional)
(: x Integer)
(define x 3)
(define x 5)

