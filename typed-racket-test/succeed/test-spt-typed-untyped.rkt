#lang racket

(module typed typed/racket
  (provide prop:foo foo?)
  (: prop:foo (Struct-Property (-> Self Number)))
  (: foo? (-> Any Boolean : (Has-Struct-Property prop:foo)))
  (: foo-ref (Exist (X) (-> (Has-Struct-Property prop:foo) (-> X Number) : X)))
  (define-values (prop:foo foo? foo-ref) (make-struct-type-property 'foo))

  (provide bar bar1 foo-ref)

  (define (bar [x : (Has-Struct-Property prop:foo)])  : Number
    10)
  (define (bar1 [x : Number]) : Number
    x)
  #;(bar (world)))


(require 'typed)
(struct world [] #:property prop:foo (lambda (self) 10))
(define x (world))
((foo-ref x) x)
