#lang scheme

(require typed/racket/shallow)

(define-values (a b)
  (with-type
   #:result (values String (Number -> Number))
   (values "foo" (lambda (x) x))))

(b a)
