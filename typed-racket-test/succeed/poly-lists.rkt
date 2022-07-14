#lang typed/racket

(define-type (Assoc X Y) (Listof (Pairof X Y)))

(: f : (Listof Integer) (Listof String) -> (Listof (Pairof Integer String)))
(define (f l m)
  (for/list : (Assoc Integer String) ([x : Integer (in-list l)]
                                      [y : String (in-list m)])
    (cons x y)))


(define (g [l : (Listof Integer)] [m : (Listof String)]) : (Assoc Integer String)
  (for/list ([x (in-list l)]
             [y (in-list m)])
    (cons x y)))
