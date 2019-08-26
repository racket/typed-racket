#;
(exn-pred " Type Checker: parse error in type")
#lang typed/racket
(: prop:foo Imp)
(define-values (prop:foo _1 _2) (make-struct-type-property 'foo))
