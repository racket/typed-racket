#;
(exn-pred #px"expected: 2\\s+given: 1")
#lang typed/racket/base
(struct (A B) foo ())
(define-type (F a) (foo a))
