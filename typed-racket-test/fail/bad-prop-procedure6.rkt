#;
(exn-pred #rx"type mismatch in the first parameter of the function for prop:procedure\n.*expected: adder-ann\n.*got: adder")
#lang typed/racket/base

(struct adder ())
(struct adder-ann ([num : Number])
  #:property prop:procedure
  (ann (λ (this n)
         (+ (adder-ann-num this) 21))
       (-> adder Number Number)))
