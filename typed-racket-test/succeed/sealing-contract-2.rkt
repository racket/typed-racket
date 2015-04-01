#lang racket/base

(module u racket
  (define (mixin cls)
    (class cls
      (super-new)
      (field [g 0])))

  (provide mixin))

(module t typed/racket
  ;; expects a mixin that adds g
  (require/typed (submod ".." u)
                 [mixin
                  (All (r #:row)
                   (-> (Class #:row-var r)
                       (Class #:row-var r (field [g Integer]))))])

  (mixin (class object%
           (super-new)
           (field [f 1]))))

(require 't)
