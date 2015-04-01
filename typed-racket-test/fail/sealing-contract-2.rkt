#;
(exn-pred #rx"disallowed members \\(m\\)")
#lang racket/base

(module u racket
  ;; adds m instead of n like the spec says
  (define (mixin cls)
    (class cls
      (super-new)
      (define/public (m x) x)))

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
