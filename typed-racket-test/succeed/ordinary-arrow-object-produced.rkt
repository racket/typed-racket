#lang typed/racket

(let ([z : Any 1])
  ((lambda ([f : (Any -> Any : #:object z)])
     (if (number? (f 0))
         (+ z 1)
         0))
   (lambda ([x : Any]) z)))
