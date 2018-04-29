#;
(exn-pred exn:fail:contract? #rx"shape-check")

#lang typed/racket/base/shallow

;; Test that `(apply f ...)` checks results safely

(module u racket/base
  (provide f2)
  (define (f2) (values 'a 'b)))

(require/typed 'u
  (f2 (-> (Values Integer Symbol))))

(: b Integer)
(: c Symbol)
(define-values [b c] (apply f2 '()))
