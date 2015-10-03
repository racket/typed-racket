#lang typed/racket

;; The call to `set-foo-x!` below should fail because the
;; predicate filter on `bar?` has to be restrictive.

(struct (A) foo ([x : A]) #:mutable)

(struct (A) baz foo ())

(define (f [i : Integer]) : (foo Integer)
    (baz i))

(: x (foo Integer))
(define x (f 1))

(: y Any)
(define y x)

(if (baz? y) (set-foo-x! y "foo") 2)
(foo-x x)
