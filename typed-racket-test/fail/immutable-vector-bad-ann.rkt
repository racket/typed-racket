#;
(exn-pred #rx"given: \\(Immutable-Vector One\\)")
#lang typed/racket

(ann (vector-immutable 1) (Mutable-Vector Real))
