#;
(exn-pred #rx"contract generation not supported for Self")
;; Ideally, this program should throw an error like "42 is not an indentical value to #<world>"
#lang racket

(module untyped racket
  (provide prop:foo foo? foo-ref)
  (define-values (prop:foo foo? foo-ref) (make-struct-type-property 'foo)))

(module typed typed/racket
  (require/typed (submod ".." untyped)
    [foo? (-> Any Boolean)]
    [prop:foo (Struct-Property (-> Self Number) foo?)])
  (provide world)
  (struct world ([x : Number]) #:property prop:foo (λ (me) (add1 (world-x me)))))

(require 'typed)
(require 'untyped)
((foo-ref (world)) 42)

