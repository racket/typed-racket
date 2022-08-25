#;
(exn-pred exn:fail:contract? #rx"add1")
#lang racket

(module t typed/racket/shallow
  (provide f g)

  (define f (ann (case-lambda [() (add1 "hello")] [(x) x]) (Number -> Number)))
  (define g (ann f Any)))

(require 't)
(f 1)
(g)
