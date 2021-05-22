#lang typed/racket/base
(struct foo ([a : Any]))
(define-type Hello (Pairof foo (Listof foo)))
(define-predicate hello? Hello)
(define #:forall (T) (hello [v : T])
  (if (and (hello? v) (foo? (car v)) (number? (foo-a (car v))))
      10
      0))
