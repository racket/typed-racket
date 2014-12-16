#lang info

(define collection 'use-pkg-name)
(define deps '("base"
               "typed-racket"
               "typed-racket-more"
               "typed-racket-compatibility"
               "unstable"
               "unstable-2d"
               "rackunit-lib"
               "racket-index"
               "pconvert-lib"
               "compatibility-lib"
               "unstable-flonum-lib"
               "unstable-list-lib"))
(define build-deps '("scheme-lib"
                     "base"
                     "racket-benchmarks"
                     "rackunit-lib"
                     "compiler-lib"
                     "redex-lib"
                     "htdp-lib"
                     "sandbox-lib"
                     "unstable"))
(define update-implies '("typed-racket"))

(define pkg-desc "tests for \"typed-racket\"")

(define pkg-authors '(samth stamourv))

(define version "1.1")


;; Collection info

(define name "Typed Racket Test Suite")

(define test-timeouts
  '(("optimizer/run.rkt" 1200)
    ("run.rkt" 1800)
    ("tr-random-testing.rkt" 300)
    ("with-tr-contracts.rkt" 1000)))


;; No need to compile the actual integration tests, just the harness.
(define compile-omit-paths
  '("succeed"
    "fail"
    "xfail"
    "optimizer" ;; FIXME: should be improved by stamourv
    "tr-random-testing.rkt" ;; Requires recompiling all of redex when rebuilding TR tests
    ))

(define test-omit-paths '("fail"
                          "xfail"))
(define test-command-line-arguments
  '(("succeed/priority-queue.scm" ())
    ("succeed/hw01.scm" ())
    ("succeed/foo.scm" ())
    ("succeed/batched-queue.scm" ())))
