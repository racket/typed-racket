#;
(exn-pred #rx"could not be converted")
#lang typed/racket/optional
(define-predicate p? (All (A) (Listof A)))
