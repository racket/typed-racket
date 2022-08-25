#;
(exn-pred exn:fail:contract? #rx"shape-check")

#lang typed/racket/base/shallow

(module u racket/base
  (define x* "OOPS")
  (provide x*))

(require/typed 'u
  (x* (Listof Integer)))

