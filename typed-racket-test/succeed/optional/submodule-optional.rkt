#lang typed/racket/base/optional

;; optional submodules in optional code

(module t typed/racket/base/optional
  (: f (-> Real (-> Real Real) Real))
  (define (f r g)
    (g (g r)))
  (provide f))
(require 't)

(: h (-> Real Real))
(define (h n)
  (* n n))

(f 3 h)
