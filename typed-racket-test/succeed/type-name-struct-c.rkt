#lang typed/racket

(provide: [bar (All (E) (-> E Integer (Bar E)))]
          [baz (All (E) (-> E Integer String (Baz E)))]
          [quux (-> (Baz Symbol) (Listof Any))])
(struct (E) bar ([x : E] [y : Integer]) #:type-name Bar)
(struct (E) baz Bar ([z : String]) #:type-name Baz)
(define (quux [baz : (Baz Symbol)]) : (Listof Any)
  (list (bar-x baz) (add1 (bar-y baz)) (baz-z baz)))
