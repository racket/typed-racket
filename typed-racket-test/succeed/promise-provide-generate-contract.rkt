#lang typed/racket

(struct (A) s ([f : Any]))
(define p (delay (s "a")))
(provide p)
