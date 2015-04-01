#lang racket/base

(module u racket
  (define state #f)

  (define (mixin cls)
    (class cls
      (super-new)
      (define/public (n x) x)))

  (provide mixin))

(module t typed/racket
  (require/typed (submod ".." u)
                 [mixin
                  (All (r #:row)
                   (-> (Class #:row-var r)
                       (Class #:row-var r [n (-> Integer Integer)])))])

  ;; ok to call mixin with different types
  (mixin (class object% (super-new)))
  (mixin (class object%
           (super-new)
           (define/public (m x) x))))

(require 't)
