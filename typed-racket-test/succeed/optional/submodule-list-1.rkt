#lang typed/racket/base/optional

;; Test importing a list with incorrect type

(module u racket/base
  (define x* (list "NaN"))
  (provide x*))

(require/typed 'u
  (x* (Listof Integer)))

(ann (append x* (list 1 2 3)) (Listof Integer))
