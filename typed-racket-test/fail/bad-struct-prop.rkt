#;
(exn-pred " Type Checker: type mismatch")
#lang typed/racket
(struct foo ([x : Number]) #:property prop:custom-write
  (lambda ([n : Number] [p : Output-Port] [m : (U Boolean 0 1)]) : Void
          (displayln (+ 10 20))))
