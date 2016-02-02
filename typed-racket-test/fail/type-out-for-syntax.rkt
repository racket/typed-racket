#lang racket/base

;; type-out only works at phase 0,
;;  because it inserts definitions at phase 0
;   in the enclosing module

(module for-stx typed/racket/base
  (require (for-syntax typed/racket/base))

  (provide
    (for-syntax (type-out [s (-> String String)])))

  (define-for-syntax (s str) ""))
(require 'for-stx)
