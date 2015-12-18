#;
(exn-pred #rx"could not be converted")
#lang racket/load

;; Tests that p? cannot be generated

(require typed/racket)

(define-predicate p? (All (A) (Listof A)))

(let ()
  (: x (U (Listof Integer) Integer)) (define x '(1 2 3))
  (if (p? x) 0 (add1 x)))
