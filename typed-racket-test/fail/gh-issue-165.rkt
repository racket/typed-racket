#;
(exn-pred #rx"match-define")
#lang typed/racket

(struct a ([x : Integer]))
(struct b ([y : Integer]))

;; test for original report
(: f (-> a Integer))
(define (f arg)
  (match-define (a (b y)) arg)
  (+ 1 y))

;; simple test case
(let ()
  (match-define (? string? x) 3)
  (+ 1 2))
