#lang typed/racket

(let ([z : Any 1])
  ((lambda ([f : (Any -> Boolean : #:+ (: z Number))])
     (if (f 0)
         (+ z 1)
         0))
   (lambda ([x : Any]) (number? z))))
