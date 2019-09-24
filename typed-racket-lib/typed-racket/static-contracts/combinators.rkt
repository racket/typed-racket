#lang racket/base

;; Reprovides everything from all the files in the combinators directory.

(define-syntax-rule (require/provide . args)
  (begin
    (require . args)
    (provide (all-from-out . args))))

(require/provide
 "combinators/any.rkt"
 "combinators/case-lambda.rkt"
 "combinators/control.rkt"
 "combinators/dep-function.rkt"
 "combinators/derived.rkt"
 "combinators/function.rkt"
 "combinators/lengths.rkt"
 "combinators/name.rkt"
 "combinators/none.rkt"
 "combinators/object.rkt"
 "combinators/parametric.rkt"
 "combinators/prefab.rkt"
 "combinators/proposition.rkt"
 "combinators/simple.rkt"
 "combinators/struct.rkt"
 "combinators/structural.rkt"
 "combinators/symbolic-object.rkt"
 "combinators/exist.rkt"
 "combinators/unit.rkt")
