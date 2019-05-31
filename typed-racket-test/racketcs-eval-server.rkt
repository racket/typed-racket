#lang racket

(require racket/sandbox)

(define ns (make-base-namespace))
(parameterize ([current-namespace ns])
  (namespace-require 'racket/math)
  (namespace-require 'racket/flonum)
  (namespace-require 'racket/unsafe/ops))

(let loop ()
  (define sexp (read))
  (unless (eof-object? sexp)
    (with-handlers ([values (lambda (e) (writeln (exn-message e)))])
      (with-limits
        5 1000 ; same as max-mem from tr-random-testing.rkt
        (writeln (eval sexp ns))))
    (loop)))
