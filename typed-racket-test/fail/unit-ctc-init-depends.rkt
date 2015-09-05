#;
(exn-pred #rx"x@: broke its own contract;\n contract does not list initialization dependency x\\^")
#lang racket


(module untyped racket
  (provide x^ x@)
  (define-signature x^ (x))
  (define x@ 
    (unit
      (import x^)
      (export)
      (init-depend x^)
      x)))

(module typed typed/racket
  (require/typed (submod ".." untyped)
    [#:signature x^ ([x : Integer])]
    [x@ (Unit (import x^)
              (export)
              Integer)]))

(require 'typed)

