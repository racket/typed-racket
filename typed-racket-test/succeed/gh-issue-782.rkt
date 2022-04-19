#lang typed/racket/base

(define x : (U String (Listof Any)) '(1))
(unless (list? x)
  (error "not a list" x))
(length x)
