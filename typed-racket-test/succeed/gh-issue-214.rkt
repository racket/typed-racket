#lang typed/racket

(struct (A B) Fum ([a : A] [b : B]))
(struct Fi ())
(struct Foo ())

(define-type Tail
  (Rec T
   (U (Fum (Listof Value) T)
       Fi)))

(define-type Value
 (Rec V
  (U (Fum (Listof Value) V)
     Foo)))

(provide fun1)

(: fun1 (-> Tail))
(define (fun1)
  (error 'foo1))
