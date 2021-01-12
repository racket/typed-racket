#;
(exn-pred 1)
#lang typed/racket

(struct root ([p : Integer] [q : Integer])
  #:property prop:custom-write
  (λ ([me : Integer] [port : Output-Port] mode) : Void
     (void)))
