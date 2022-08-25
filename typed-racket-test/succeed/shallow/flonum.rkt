#lang racket/base

(module t typed/racket/base/shallow
  ;; Test case-> type
  (require racket/flonum)
  (provide flprobability?)
  (: flprobability? (case-> (Flonum -> Boolean) (Flonum Any -> Boolean)))
  (define (flprobability? p [log? #f])
    (cond [log?  (and (p . fl>= . -inf.0) (p . fl<= . 0.0))]
          [else  (and (p . fl>= . 0.0) (p . fl<= . 1.0))])))

(require 't rackunit)

(check-not-exn
  (lambda () (flprobability? 0.5)))

(check-exn #rx"shape-check"
  (lambda () (flprobability? "seven")))
