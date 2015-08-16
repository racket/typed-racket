#lang typed/racket

(define-new-subtype F (make-F (-> Real Real)))

(: app : F Real -> Real)
(define (app f x)
  (f x))

