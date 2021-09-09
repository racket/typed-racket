#;
(exn-pred " Type Checker: parse error in type")
#lang typed/racket
(struct (X) foo ([a : X]) #:property prop:custom-write (λ ([self : foo] [p : Output-Port] [m : (U 0 1 Boolean)])
                                                         (display 'ha)))
