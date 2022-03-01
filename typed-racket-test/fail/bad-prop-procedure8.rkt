#;
(exn-pred #rx"type mismatch.*expected: \\(-> adder Number Number\\).*given: String")
#lang typed/racket/base

(struct adder ([num : Number])
  #:property prop:procedure
  (ann "hello"
       (-> adder Number Number)))
