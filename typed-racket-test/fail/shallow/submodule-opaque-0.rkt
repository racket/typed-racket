#;
(exn-pred exn:fail:contract? #rx"shape-check")

#lang typed/racket/base/shallow

(module u racket/base
  (define x* "not function")
  (provide x*))

(require/typed 'u
  (#:opaque X? x*))

