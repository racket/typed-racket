#lang typed/racket
(struct (A) S ([f : A]))

(define-type T (∩ (S Nonnegative-Integer) (S Nonpositive-Integer)))
