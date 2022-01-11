#lang racket

(require racket/sandbox)

(define ns (make-base-namespace))
(parameterize ([current-namespace ns])
  (namespace-require 'racket/math)
  (namespace-require 'racket/flonum)
  (namespace-require 'racket/unsafe/ops))

(eval '(define (safe-arithmetic-shift a b)
         (if (> b 100000) b (arithmetic-shift a b)))
      ns)

(let loop ([n 0])
  (when (= 0 (modulo n 50))
    (collect-garbage))
  (define sexp (read))
  (unless (eof-object? sexp)
    (with-handlers ([values (lambda (e) (writeln (list n
                                                       (exn-message e)
                                                       (continuation-mark-set->context
                                                        (exn-continuation-marks e)))))])
      (with-limits
        10 2000 ; same as max-mem from tr-random-testing.rkt
        (writeln (vector (eval sexp ns) sexp))))
    (loop (add1 n))))
