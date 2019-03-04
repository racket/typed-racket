#lang typed/racket
(struct bar ([x : Number]))

(struct foo ([x : Number]) #:property prop:custom-write
  (lambda ([n : foo] [p : Output-Port] [b : Boolean]) : Void
          (displayln (+ 10 20))))
