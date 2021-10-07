#;
(exn-pred #rx"expected a structure type for argument to Struct")
#lang typed/racket

;; Make sure `Struct` constructor rejects bad arguments
(: x (Struct Integer))
(define x 3)

