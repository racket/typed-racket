#;
(exn-pred #rx"wrong number of arguments to polymorphic type")
#lang typed/racket

;; Check bad arity for recursive invocation of Foo

(define-type (Foo A) (Listof (Foo A A)))

