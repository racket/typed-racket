#lang typed/racket

(require typed/rackunit
         typed/rackunit/text-ui)

(define passing-suite
  (test-suite "Example Typed Rackunit test suite"
    (check-equal? 1 1)))

(check-equal? (run-tests passing-suite 'quiet) 0)
