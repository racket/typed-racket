#lang typed/racket

(: assert-positive ((U Integer String) -> Integer))
(define (assert-positive x) (assert x integer?))

(: assert-negative ((U Integer String) -> Integer))
(define (assert-negative x)
  (assert x (lambda ([v : Any]) (not (string? v)))))

(: assert-truthy ((U False Integer) -> Integer))
(define (assert-truthy x) (assert x))

(: with-positive ((U Integer String) -> Integer))
(define (with-positive x) (with-asserts ([x integer?]) x))

(: with-negative ((U Integer String) -> Integer))
(define (with-negative x)
  (with-asserts ([x (lambda ([v : Any]) (not (string? v)))]) x))

(: with-truthy ((U False Integer) -> Integer))
(define (with-truthy x) (with-asserts ([x]) x))

;; Both assertions must contribute refinements to the body.
(: with-sequential ((U False Integer String) -> Integer))
(define (with-sequential x)
  (with-asserts ([x] [x (lambda ([v : Any]) (not (string? v)))]) x))

(for ([f (in-list (list assert-positive assert-negative assert-truthy
                        with-positive with-negative with-truthy with-sequential))])
  (unless (= (f 123) 123)
    (error 'assert-refinement "unexpected assertion result")))
