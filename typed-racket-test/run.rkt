#lang racket

;; Test mode:
(module test racket/base
  (require syntax/location)
  (parameterize ([current-command-line-arguments (vector "--nightly")])
    (dynamic-require '(submod typed-racket-test/main main) 0))
  (module config info
    (define timeout 1800)))
