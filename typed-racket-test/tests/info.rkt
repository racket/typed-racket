#lang info

(define name "Typed Racket Test Suite")

(define test-timeouts
  '(("typed-racket/typed-racket/optimizer/run.rkt" 1200)
    ("typed-racket/typed-racket/run.rkt" 1800)
    ("typed-racket/typed-racket/tr-random-testing.rkt" 300)
    ("typed-racket/typed-racket/with-tr-contracts.rkt" 1000)
    ))


;; No need to compile the actual integration tests, just the harness.
(define compile-omit-paths
  '("typed-racket/succeed"
    "typed-racket/fail"
    "typed-racket/xfail"
    "typed-racket/optimizer" ;; FIXME: should be improved by stamourv
    "typed-racket/tr-random-testing.rkt" ;; Requires recompiling all of redex when rebuilding TR tests
    ))
