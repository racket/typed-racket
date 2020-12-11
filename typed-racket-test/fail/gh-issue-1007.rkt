#;
(exn-pred " Type Checker: Polymorphic function `sort' could not be applied to arguments")
#lang typed/racket #:no-delay-errors

(sort (list (cons 10 20) (cons 20 30)) <=)
