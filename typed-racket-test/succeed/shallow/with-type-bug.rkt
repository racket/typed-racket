#lang scheme
(require (prefix-in T: typed/racket/shallow))
((T:with-type #:result (T:Integer T:-> T:Integer) add1) 1/2)
