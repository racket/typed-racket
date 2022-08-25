#lang typed/racket/base/shallow

(define sym* : (Listof Symbol) '(A B C))

(call-with-values (lambda () sym*) car)
(call-with-values (lambda () sym*) reverse)

