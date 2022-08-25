#lang typed/racket/base/shallow

;; shallow submodules of shallow mod

(module t typed/racket/base/shallow
  (: f (-> Real (-> Real Real) Real))
  (define (f r g)
    (g (g r)))
  (provide f))
(require 't)

(: h (-> Real Real))
(define (h n)
  (* n n))

(f 3 h)
