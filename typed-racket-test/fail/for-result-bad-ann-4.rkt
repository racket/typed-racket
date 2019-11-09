#lang typed/racket
(for*/fold : (Values Integer Integer)
           ([x : Integer 0]
            [y : Integer 0]
            #:result (+ x y))
           ([i (in-range 3)]
            [j (in-range 3)])
  (values (+ x i) (+ y i j)))
