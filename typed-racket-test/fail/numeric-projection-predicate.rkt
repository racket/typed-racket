#;
(exn-pred "mismatch in proposition")
#lang typed/racket/base
(: wrong (Any -> Boolean : Float-Complex))
(define (wrong x) (and (number? x) (flonum? (real-part x))))
