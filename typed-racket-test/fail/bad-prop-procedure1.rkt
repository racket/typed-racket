#;
(exn-pred #rx"expected: Procedure\n.*given: String")
#lang typed/racket/base
(struct some-struct1 ([a : String] [b : (-> Number Number)]) #:property prop:procedure 0)
