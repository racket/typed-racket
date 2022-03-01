#;
(exn-pred #rx".*expected: Number\n.*given: Any")

#lang typed/racket/base

(struct adder^ ([num : Number])
  #:property prop:procedure
  (λ ([this : adder^])
    (+ (adder^-num this) 21)))

(add1 ((adder^ 42)))
