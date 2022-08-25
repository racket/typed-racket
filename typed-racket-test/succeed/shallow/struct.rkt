#lang typed/racket/shallow

;; Test structs: definition, creation, accessors

(struct A ((x : Integer) (y : (Listof Integer)) (z : (-> Integer Integer))))

(define a (A 1 '(1) (lambda ((x : Integer)) (+ x x))))

(+ ((A-z a) (A-x a)) (car (A-y a)))

((lambda (x) x) 3)


(: f (-> (Listof A) Integer))
(define (f x)
  (define mya (car x))
  (A-x mya))

(f (list a))
