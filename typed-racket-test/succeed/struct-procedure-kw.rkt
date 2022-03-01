#lang typed/racket/base

(struct adder-kw-opt ([num : Number])
  #:property prop:procedure
  (lambda ([this : adder-kw-opt] #:hello [n : Number 1]) : Number
    (* (adder-kw-opt-num this) n)))

(let ([akw (adder-kw-opt 42)])
  (akw #:hello 21)
  (akw))

(struct (A) poly2 ([a : A])
  #:property prop:procedure
  (lambda ([this : (poly2 A)]
           #:extra
           [a : Integer] [li : (Listof A)]) : Integer
    (+ a (length
          (cons (poly2-a this)
                li)))))

(define poly-ins^ (poly2 42))
(add1 (poly-ins^ #:extra 42 '(1 2 3)))
