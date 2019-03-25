#;
(exn-pred " Type Checker: type mismatch")
#lang typed/racket
(struct foo ([x : Number]))

(struct dummy ([y : Number]) #:property prop:custom-write
  (lambda ([self : dummy] [p : Output-Port] [b : Boolean]) : Void
          (displayln (+ (foo-x self) 20))))
