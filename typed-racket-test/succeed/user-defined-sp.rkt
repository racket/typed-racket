#lang typed/racket

(: prop:foo (Struct-Property (-> Self Number)))
(: _1 (-> Any Boolean))
(: _2 (-> Any Any))
(define-values (prop:foo _1 _2) (make-struct-type-property 'foo))

(struct use-foo () #:property prop:foo (λ ([this : use-foo]) 10))


(: prop:super-bar (Struct-Property Real))
(: __1 (-> Any Boolean))
(: __2 (-> Any Any))
(define-values (prop:super-bar __1 __2) (make-struct-type-property 'super-bar))

(: prop:bar (Struct-Property Integer))
(define prop:bar prop:super-bar)

(struct use-bar () #:property prop:bar 10)
