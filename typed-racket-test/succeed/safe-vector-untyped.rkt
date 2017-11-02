#lang racket/base

(require "safe-vector.rkt")

(define len 50)
 
(define (run-safe-ref-test)
  (define vec (make-vector len 0))
  (collect-garbage)
  (collect-garbage)
  (collect-garbage)
  (time (for*/sum ([_ (in-range 10000)]
                   [i (in-range len)])
          (safe-vector-ref vec i))))

(define (run-safe-set!-test)
  (define vec (make-vector len 0))
  (collect-garbage)
  (collect-garbage)
  (collect-garbage)
  (time (for* ([_ (in-range 10000)]
               [i (in-range len)])
          (safe-vector-set! vec i 0))))



;; don't let the optimizer get too excited (try and hide
;; exactly what the ref and set functions are)
(define vref #f)
(set! vref vector-ref)
(define vset! #f)
(set! vset! vector-set!)

(define (run-reg-ref-test)
  (define vec (make-vector len 0))
  (collect-garbage)
  (collect-garbage)
  (collect-garbage)
  (time (for*/sum ([_ (in-range 10000)]
                   [i (in-range len)])
          (vref vec i))))

(define (run-reg-set!-test)
  (define vec (make-vector len 0))
  (collect-garbage)
  (collect-garbage)
  (collect-garbage)
  (time (for* ([_ (in-range 10000)]
                   [i (in-range len)])
          (vset! vec i 0))))


(printf "vector-ref:\n")
(run-reg-ref-test)
(printf "safe-vector-ref:\n")
(run-safe-ref-test)
(printf "vector-set!:\n")
(run-reg-set!-test)
(printf "safe-vector-set!:\n")
(run-safe-set!-test)

; Example results
; Racket v6.10.1.1
; with an Intel(R) Core(TM) i5-7260U CPU @ 2.20GHz:
;
;vector-ref:
;cpu time: 8 real time: 6 gc time: 0
;0
;safe-vector-ref:
;cpu time: 232 real time: 233 gc time: 0
;0
;vector-set!:
;cpu time: 8 real time: 6 gc time: 0
;safe-vector-set!:
;cpu time: 292 real time: 293 gc time: 0
