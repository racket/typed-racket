#lang info

(define collection 'use-pkg-name)
(define deps '("redex-lib"
               "sandbox-lib"
               ("base" #:version "6.2.900.16")
               ("typed-racket-lib" #:version "1.10")
               "typed-racket-more"
               "typed-racket-compatibility"
               "2d"
               "rackunit-lib"
               "racket-index"
               "compatibility-lib"
               "math-lib"))
(define build-deps '("scheme-lib"
                     "base"
                     "racket-benchmarks"
                     "rackunit-lib"
                     "compiler-lib"
                     "redex-lib"
                     "htdp-lib"
                     "sandbox-lib"))
(define update-implies '("typed-racket-lib"
                         "typed-racket-more"
                         "typed-racket-compatibility"))

(define pkg-desc "tests for \"typed-racket\"")

(define pkg-authors '(samth stamourv endobson asumu "andmkent@iu.edu"))

(define version "1.10")


;; Collection info

(define name "Typed Racket Test Suite")

(define test-timeouts
  '(("optimizer/run.rkt" 1200)
    ("run.rkt" 1800)
    ("with-tr-contracts.rkt" 3000)))


;; No need to compile the actual integration tests, just the harness.
(define compile-omit-paths
  '("succeed"
    "fail"
    "xfail"
    "optimizer" ;; FIXME: should be improved by stamourv
    "tr-random-testing.rkt" ;; Requires recompiling all of redex when rebuilding TR tests
    "gui/succeed"
    ))

(define test-omit-paths '("fail"
                          "xfail"))
(define test-command-line-arguments
  '(("succeed/priority-queue.scm" ())
    ("succeed/hw01.scm" ())
    ("succeed/foo.scm" ())
    ("succeed/batched-queue.scm" ())))
