#;
(exn-pred exn:fail:syntax? #rx"free variables")

#lang racket/load

(require typed/racket/base/shallow)

(define: (a) (f (x : Number)) : a
  (cast x a))
