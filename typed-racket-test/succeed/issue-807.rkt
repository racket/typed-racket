#lang typed/racket
;; https://github.com/racket/typed-racket/issues/807

(random-seed 1234)

(define r (current-pseudo-random-generator))

(define (run-trials [trials : Natural])
  (for ([i (in-range trials)])
    (define a (assert (random r) positive?))
    (define b (assert (random r) positive?))
    (define c (assert (random r) positive?))
    (if (and (< (abs (- a b)) c)
             (< c (sqrt (+ (* a a) (* b b)))))
      1
      0)))

(run-trials (expt 2 20))
