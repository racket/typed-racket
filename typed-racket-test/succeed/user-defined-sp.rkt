#lang typed/racket

(: prop:foo (Struct-Property (-> Self Number)))
(: _1 (-> Any Boolean : (Has-Struct-Property prop:foo)))
(: _2 (Exist (A) (-> (Has-Struct-Property prop:foo) (-> A Number) : A)))
(define-values (prop:foo _1 _2) (make-struct-type-property 'foo))

(struct use-foo () #:property prop:foo (λ ([this : use-foo]) 10))


(: prop:super-bar (Struct-Property Real))
(: __1 (-> Any Boolean : (Has-Struct-Property prop:super-bar)))
(: __2 (-> (Has-Struct-Property prop:super-bar) Real))
(define-values (prop:super-bar __1 __2) (make-struct-type-property 'super-bar))

(: prop:bar (Struct-Property Integer))
(define prop:bar prop:super-bar)

(struct use-bar () #:property prop:bar 10)
