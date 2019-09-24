#lang racket/base
(provide eq/c)
(require racket/contract/combinator)

(define (eq/c name)
  (make-contract #:name (format "not an identical value to ~a" name)
                 #:first-order (λ (y)
                                 (eq? name y))))



    
