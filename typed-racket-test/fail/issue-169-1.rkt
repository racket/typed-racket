#;
(exn-pred #rx"expected: \\Null.*given: \\(Listof Pos\\*\\)")
#lang typed/racket
(define-type Pos Integer)
(define-new-subtype Pos* (p Pos))
(define lst
  (for*/list ([x (in-range 3)]) : (Listof Pos*)
    (p x)))
(ann lst (Listof Nothing)) ; this should fail
