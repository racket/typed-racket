#;
(exn-pred #rx"given: \\(Mutable-Vector Integer\\)")
#lang typed/racket

(ann (vector 1) (Immutable-Vectorof Real))
