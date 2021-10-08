#;
(exn-pred 2)
#lang typed/racket

;; Make sure `Struct` constructor rejects bad arguments
(: x (Struct Integer))
(define x 3)

