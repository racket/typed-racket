#;
(exn-pred " Type Checker: parse error in type;")
#lang typed/racket
(struct a-struct-with-construct-id ([x : Number])
  #:constructor-name ACons
  #:property prop:custom-write
  (lambda ([self : ACons] [p : Output-Port] [m : (U Boolean 1 0)]) : Void
          (displayln (+ (a-struct-with-construct-id-x self) 20))))
