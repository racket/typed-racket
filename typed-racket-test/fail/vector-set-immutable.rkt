#;
(exn-pred exn:fail?)
#lang typed/racket

(define v : (Vectorof Integer) (vector-immutable 1 2 3))
(vector-set! v 0 0)
