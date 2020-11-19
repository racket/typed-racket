#lang typed/racket

(: bar (-> (-> Natural * Any) Any))
(define (bar f)
  'any)

(: foo (-> (->* () (Integer Integer) #:rest Natural Any)
           Any))
(define (foo f)
  (bar f))

(: foo^ (-> Any * Any))
(define foo^
  (case-lambda
    [() 'one]
    [(a) 'two]
    [(a b . cs) 'three]))

