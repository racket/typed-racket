#lang typed/racket/base
(provide x)
(define x : Integer 1)
(module* test racket/base
  (require (submod ".."))
  x)
