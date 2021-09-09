#;
(exn-pred #rx"bad syntax in type application: expected a type constructor")
#lang typed/scheme



(define-type T+
  (All (elem)
       (U (A elem))))

(define-type T
  (All (elem)
       (U (T+ elem) 2)))

(define-struct: (x) A ())
