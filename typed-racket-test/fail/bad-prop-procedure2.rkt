#;
(exn-pred #rx"index too large\n.*index: 3\n.*maximum allowed index: 1")
#lang typed/racket/base
(struct some-struct1 ([a : String] [b : (-> Number Number)]) #:property prop:procedure 3)
