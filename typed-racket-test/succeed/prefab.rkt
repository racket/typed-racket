#lang typed/racket

;; Test prefab struct declarations

(struct foo ([x : Symbol]) #:prefab)
(struct bar foo ([y : String] [z : String]) #:prefab)
(define-struct foo* ([x : Symbol]) #:prefab)
(define-struct (bar* foo*) ([y : String] [z : String]) #:prefab)

(: a-bar (Prefab (bar foo 1) Symbol String String))
(define a-bar (bar 'foo "bar1" "bar2"))

(foo-x (foo 'foo))
(bar-y (bar 'foo "bar1" "bar2"))

(foo*-x (make-foo* 'foo))
(bar*-y (make-bar* 'foo "bar1" "bar2"))

;; prefab keys may be normalized or not
(: a-bar-2 (Prefab (bar 2 foo 1 (0 #f) #()) Symbol String String))
(define a-bar-2 (bar 'foo "bar1" "bar2"))

;; prefab subtyping is computed via the key and field length
(: a-bar-3 (Prefab foo Symbol))
(define a-bar-3 (bar 'foo "bar1" "bar2"))

;; Mutable prefab structs

(struct baz ([x : String]) #:mutable #:prefab)
(define-struct baz* ([x : String]) #:mutable #:prefab)

(define a-baz (baz "baz"))
(set-baz-x! a-baz "baz2")
(baz-x a-baz)

(define a-baz* (make-baz* "baz"))
(set-baz*-x! a-baz* "baz2")
(baz*-x a-baz*)

;; Polymorphic prefab structs

(struct (X) poly ([x : X]) #:prefab)
(define-struct (X) poly* ([x : X]) #:prefab)

(poly-x (poly "foo"))
(poly-x (poly 3))
(poly-x #s(poly "foo"))

(poly*-x (make-poly* "foo"))
(poly*-x (make-poly* 3))
(poly*-x #s(poly* "foo"))

;; Test match (indirectly tests unsafe-struct-ref)
(match (foo 'x) [(foo s) s])
(match (foo* 'x) [(foo* s) s])
