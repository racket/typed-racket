#lang typed/racket/shallow

;; Test inspired by the suffixtree benchmark.
;; Function returns a union of polymorphic variables and should have no check

(: f (All (A B) (-> (-> A) (-> B) (U A B))))
(define (f a b)
  (: helper (-> Integer (U A B)))
  (define (helper n)
    (if (zero? n) (a) (b)))
  (helper 0))

(f (lambda () #true) (lambda () 'false))
