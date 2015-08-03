#;
(exn-pred #rx"type mismatch")
#lang typed/racket

(require (for-syntax syntax/parse))


(unit (import)
      (export)
      (define-syntax (my-define stx)
        (syntax-parse stx
          [(_ name:id #:with-type type expr)
           #'(begin
               (: name type)
               (define name expr))]))
      (my-define bad #:with-type Integer "BAD"))
