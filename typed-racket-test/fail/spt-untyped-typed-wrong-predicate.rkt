#;
(exn-pred exn:fail:contract? #rx"struct-property: predicate does not match property")
#lang racket

(module untyped racket
  (provide prop:foo)
  (define-values (prop:foo foo? foo-ref) (make-struct-type-property 'foo)))

(module typed typed/racket
  (require/typed (submod ".." untyped)
    [prop:foo (Struct-Property Number fake-foo?)])
  (provide prop:foo bar)
  #;(provide bar)

  (define (fake-foo? x)
    #t)

  (define (bar [x : (Has-Struct-Property prop:foo)])  : Number
    10)
  #;(bar (world)))


(require 'typed)
(struct world [] #:property prop:foo 10)
(bar (world))
