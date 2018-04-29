#lang typed/racket/base/shallow

;; Test that nested elimination forms get checked

(define (f)
  (car (car '((1)))))
