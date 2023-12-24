#lang info

(define collection 'use-pkg-name)
(define deps '("redex-lib"
               "sandbox-lib"
               ("base" #:version "6.2.900.16")
               ("typed-racket-lib" #:version "1.10")
               "typed-racket-more"
               "2d"
               "typed-racket-compatibility"
               "racket-index"
               "rackunit-lib"
               "compatibility-lib"
               "racket-test-core"
               ))
(define build-deps '("scheme-lib"
                     "base"
                     "racket-benchmarks"
                     "compiler-lib"
                     "htdp-lib"
                     ))
(define update-implies '("typed-racket-lib"
                         "typed-racket-more"
                         "typed-racket-compatibility"))

(define pkg-desc "tests for \"typed-racket\"")

(define pkg-authors '(samth stamourv endobson "drdr@syntaxforge.net"))

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
    "external"
    "fail"
    "unit-tests/shallow-rewrite-expansion"
    "xfail"
    "racketcs-eval-server.rkt"
    "optimizer" ;; FIXME: should be improved by stamourv
    "gui/succeed"))

(define test-omit-paths '("fail"
                          "external/fail"
                          "unit-tests/shallow-rewrite-expansion"
                          "xfail"))
(define test-command-line-arguments
  '(("succeed/priority-queue.scm" ())
    ("succeed/hw01.scm" ())
    ("succeed/foo.scm" ())
    ("succeed/batched-queue.scm" ())))

(define license
  '(Apache-2.0 OR MIT))
