#lang typed/racket

(: a (-> (Some (a) (-> a)) Void))

(define (a sizeable)
  (sizeable)
  (void))