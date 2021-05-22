#lang typed/racket/base
(struct (T) bar ([a : T]))

(define (why2 [v : Any])
  (if (and (pair? v) (bar? (car v)) (string? (bar-a (car v))))
      42
      0))
