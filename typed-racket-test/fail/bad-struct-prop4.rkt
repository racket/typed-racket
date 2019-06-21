#;
(exn-pred " Type Checker: type mismatch")
#lang typed/racket
(struct (T) equal-foo ([x : T]) #:mutable
  #:property prop:equal+hash
  (list
   (lambda ([self : (equal-foo T)] [other : (equal-foo T)] [conv : (-> Any Any Boolean)]) : Any
             10)
   (lambda ([self : (equal-foo T)] [conv : (-> Any Integer)]) : Integer
           10)
   (lambda ([self : (equal-foo T)] [conv : (-> Any Integer)]) : Integer
           10)))
