#lang typed/racket
(lambda #:forall (A) ([a : A] #:mand mand) 10)

(lambda #:forall (A) ([a : A] #:mand [mand : (Listof A)]) 10)

(define #:forall (A) (bar [a : (Listof A)] #:mand [mand : Integer]) : Integer
  0)
