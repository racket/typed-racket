#;
(exn-pred #rx"expected: Procedure or nonnegative integer literal")
#lang typed/racket/base
(struct some-struct1 ([a : String] [b : (-> Number Number)]) #:property prop:procedure -1)
