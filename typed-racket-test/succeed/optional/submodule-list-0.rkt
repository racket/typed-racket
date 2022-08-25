#lang typed/racket/base/optional

;; Test importing a list

(module u racket/base
  (define x* (list 1))
  (provide x*))

(require/typed 'u
  (x* (Listof Integer)))

(+ (car x*) 1)
