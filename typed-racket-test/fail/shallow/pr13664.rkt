#;
(exn-pred exn:fail:contract? #rx"shape-check")
#lang racket/load

(module untyped racket
  (provide f)
  (define (f g)
    (g "foo")))


(module typed typed/racket/shallow
  (require/typed 'untyped
    [f (Procedure -> Any)])

  (: g (Byte -> Natural))
  (define (g x) (add1 x))

  (f g))

(require 'typed)
