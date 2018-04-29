#lang typed/racket/base

;; basic shallow -> deep, is ok

(module shallow typed/racket/base/shallow
  (provide xxx)
  (define xxx '$$$)
  xxx)

(require 'shallow)
xxx

(define (f y)
  xxx)

