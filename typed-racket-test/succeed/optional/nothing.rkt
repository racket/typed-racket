#lang typed/racket/base/optional

;; type `Nothing` should get no check, like everything else

(module a racket/base
  (provide f)
  (define (f) 42))

(require/typed 'a
  (f (-> Nothing)))

(f)
