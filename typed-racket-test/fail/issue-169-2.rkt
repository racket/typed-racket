#;
(exn-pred #rx"expected: \\(Listof Nothing\\).*given: \\(Listof Pos\\*\\)")
#lang typed/racket
(define-type Pos Integer)
(define-new-subtype Pos* (p Pos))
(define lst : (Listof Pos*)
  '())
(define lst*
  (reverse lst))
(ann lst* (Listof Nothing)) ; this should fail
