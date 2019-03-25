#;
(exn-pred " Type Checker: type mismatch")
#lang typed/racket
(struct foo ([x : Number]) #:property prop:custom-write
  (lambda ([n : Number] [p : Output-Port] [b : Boolean]) : Void
          (displayln (+ 10 20))))
