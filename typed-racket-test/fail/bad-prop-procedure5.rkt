#;
(exn-pred #rx"type mismatch in the domain of the value for prop:procedure\n.*expected: some-struct1\n.*got: Number")
#lang typed/racket/base
(struct some-struct1 () #:property prop:procedure (lambda ([not-me : Number]) : Number
                                                    (add1 not-me)))
