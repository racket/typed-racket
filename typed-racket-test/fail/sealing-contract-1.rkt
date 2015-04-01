#;
(exn-pred #rx"an unrelated class")
#lang racket/base

(module u racket
  ;; drops cls on the floor, doesn't match spec
  (define (mixin cls)
    (class object% (super-new)))

  (provide mixin))

(module t typed/racket
  ;; expects a mixin that adds n
  (require/typed (submod ".." u)
                 [mixin
                  (All (r #:row)
                   (-> (Class #:row-var r)
                       (Class #:row-var r [n (-> Integer Integer)])))])

  (mixin (class object%
           (super-new)
           (define/public (m x) x))))

(require 't)
