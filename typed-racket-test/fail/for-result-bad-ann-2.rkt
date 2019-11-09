#lang typed/racket
(for*/lists : (Values (Listof Integer) (Listof Integer))
            ([l1 : (Listof Integer)]
             [l2 : (Listof Integer)]
             #:result (append l1 l2))
            ([x (in-range 3)])
  (values x x))
