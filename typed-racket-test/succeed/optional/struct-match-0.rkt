#lang racket/base

(module s typed/racket/base/optional
  (require racket/match)

  (struct posn ([x : Real] [y : Real]))

  (: f (-> posn Void))
  (define (f p)
    (match p
     ((posn x y) (void (+ x y)))
     (_ (void)))))

(require 's)

