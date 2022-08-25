#lang racket/base

(module t typed/racket/base
  (struct posn ([x : Real] [y : Real]))
  (provide (struct-out posn)))

(module s typed/racket/base/optional
  (require racket/match
           (submod ".." t))
  (: f (-> posn Void))
  (define (f p)
    (match p
     ((posn x y) (void (+ x y)))
     (_ (void)))))

(require 's)
