#;
(exn-pred #rx"mismatch in object")
#lang typed/racket

(let ([z : Any 1]
      [w : Any 2])
  (ann (lambda ([x : Any]) w)
       (Any -> Any : #:object z)))
