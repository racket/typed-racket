#lang typed/racket
(provide
 (contract-out
  [f f-ctc]))

(: f (-> Integer Integer))
(define (f n)
  (+ 1 n))

(: f-ctc (Con (-> Integer Any) (-> Any Integer)))
(define f-ctc (->/c exact-integer? exact-integer?))
