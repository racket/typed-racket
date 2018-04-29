#;
(exn-pred exn:fail:syntax? #rx"free variables")

#lang racket/load

(require typed/racket/base/optional)

(define: (a) (f (x : Number)) : a
  (cast x a))
