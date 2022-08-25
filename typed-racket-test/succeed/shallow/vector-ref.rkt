#lang typed/racket/shallow

;; Test successful vector-ref operations

(: f (-> (Vectorof Natural) Natural))
(define (f x)
  (+ (vector-ref x 0)
     (vector-ref x 1)))

(f (vector 4 4 4 4))

(: g (-> (Vectorof (Vectorof Natural)) Natural))
(define (g x)
  (define v (vector-ref x 0))
  (+ (vector-ref (vector-ref x 0) 0) (vector-ref v 1)))

(g (vector (vector 1 2 2 3 4)))

(: h (-> (Vector Natural Symbol) Natural))
(define (h x)
  (vector-ref x 0))

(h (vector 1 'asdf))
