#;
(exn-pred exn:fail:contract? #rx"shape-check")
#lang scheme/load

(module T typed/racket/shallow

  (define-struct: [a] thing ([get : a]))

  (: thing->string ((thing String) -> String))
  (define (thing->string x)
    (string-append "foo" (thing-get x)))

  (provide (all-defined-out)))

(module U scheme

  (require 'T)

  (thing->string (make-thing 5)))

(require 'U)
