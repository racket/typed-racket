#lang typed/racket

(provide f)

(: f (-> (U Symbol String) Void))
(define (f x)
  (void))
