#lang typed/racket
(for/fold : Integer
           ([x : Integer 0]
            #:result (number->string x))
           ([y (in-range 3)])
  (+ x y))
