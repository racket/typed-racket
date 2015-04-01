#lang racket/base

(module u racket
  (define (mixin cls)
    (class cls
      (super-new)
      (define/public (n x) (add1 x))))

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
