#lang racket/base

;; Make sure contracts for GUI types can be generated in a
;; reasonable amount of time and space.

(require racket/sandbox)
(require racket/gui/base)
(define-namespace-anchor a)
(define ns (namespace-anchor->namespace a))

(call-with-limits
 120
 500
 (λ () (eval '(begin (module a typed/racket
                       (require typed/racket/gui)
                       (define x (make-object bitmap% 100 100))
                       (provide x))
                     (require 'a)
                     x)
             ns)))
