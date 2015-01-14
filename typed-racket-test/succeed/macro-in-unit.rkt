#lang typed/racket


(require (for-syntax syntax/parse))

(define-signature x^ ([x : Integer] [y : String] [z : (-> Symbol)]))

(unit (import)
      (export x^)
      (define-syntax (my-define stx)
        (syntax-parse stx
          [(_ name:id expr:exact-integer)
           #'(begin
               (: name Integer)
               (define name expr))]
          [(_ name:id expr:str)
           #'(begin
               (: name String)
               (define name expr))]
          [(_ name:id #:with-type type expr)
           #'(begin
               (: name type)
               (define name expr))]))
      (my-define x 17)
      (my-define a "hello")
      (my-define y "a string")
      (my-define z #:with-type (-> Symbol) (λ () (string->symbol y))))
