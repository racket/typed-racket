#;
(exn-pred #rx"expected: a nonnegative integer literal or an annotated lambda")
#lang typed/racket/base
(struct some-struct1 ([a : String] [b : (-> Number Number)]) #:property prop:procedure -1)
