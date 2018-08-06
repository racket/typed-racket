#;
(exn-pred exn:fail:contract? "expected: a number strictly greater than 5")
#lang racket
(module ctc-ho-pro typed/racket
  (provide (contract-out
            [foo (->/c (>/c 5) (>/c 5))]))
  (: foo (-> Real Real))
  (define (foo x) x))
(module typed typed/racket
  (require (submod ".." ctc-ho-pro))
  (foo 4))
(require 'typed)
