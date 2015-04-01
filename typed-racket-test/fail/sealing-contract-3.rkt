#;
(exn-pred #rx"disallowed members \\(f\\)")
#lang racket/base

(module u racket
  ;; adds f instead of g like the spec says
  (define (mixin cls)
    (class cls
      (super-new)
      (field [f 0])))

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
