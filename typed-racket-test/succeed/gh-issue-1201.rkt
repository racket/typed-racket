#lang typed/racket/base

(define a 10)
;; the type checker recognizes the following pattern as an opt lambda clause
;; see how `aux-table` is created in the function tc/lambda-clause for details.
(define (pos)
  (let-values ([(a) (if #f 10 a)])
    10))
(pos)
