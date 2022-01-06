#;
(exn-pred #rx"expected: Radians.*given: Degrees")
#lang typed/racket/base

(require "../succeed/define-new-subtype.rkt")

(sin (degrees 0))
