#lang typed/racket/base/optional

(module a racket/base
  (provide f)
  (define (f acc i)
    'not-int))

(require/typed 'a
  (f (-> Integer Integer Integer)))

(define x : (U #f Integer)
  (foldl f 0 '(0 1 2)))

