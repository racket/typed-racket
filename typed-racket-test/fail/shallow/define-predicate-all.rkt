#;
(exn-pred #rx"could not be converted")

#lang typed/racket/shallow

(require typed/rackunit)

(define-predicate p? (All (A) (Listof A)))
