#;
(exn-pred "define-struct: expected typed structure type options")
#lang typed/racket/base

(define-struct foo ()
               ;; can't have both of these
               #:constructor-name foo-cn
               #:extra-constructor-name foo-ecn)
