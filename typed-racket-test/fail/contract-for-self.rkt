#;
(exn-pred #rx"contract generation not supported for Self")
#lang racket

(module untyped racket
  (provide prop:foo foo? foo-ref)
  (define-values (prop:foo foo? foo-ref) (make-struct-type-property 'foo)))

(module typed typed/racket
  (require/typed (submod ".." untyped)
    [foo? (-> Any Boolean)]
    [prop:foo (Struct-Property (-> Self Number) foo?)]
    [foo-ref (Some (X) (-> (Has-Struct-Property prop:foo) (-> X Number) : X))]))
