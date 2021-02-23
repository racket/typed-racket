#lang typed/racket
(define buffer (make-bytes 0))
(define ret-val (read-bytes-avail! buffer))
(cond [(eof-object? ret-val) 0]
      [(exact-positive-integer? ret-val) 0]
      [(procedure? ret-val) (ret-val 1 1 2 3)])
