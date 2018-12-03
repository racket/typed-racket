#lang typed/racket

; Test for Github issue 506
; https://github.com/racket/typed-racket/issues/506

; Struct with #:type-name can be inherited:
(struct a ([f : Integer]) #:type-name A)
(struct b a ())
(void (list
       (ann (a 1) A)
       (ann (b 1) A)
       (ann (a 1) a)
       (ann (b 1) a)))

; Test with a polymorphic parent also:
(struct (T) p ([a : T]) #:type-name P)
(struct (T) q p ())
(void (list
       (ann (p 1) (P Integer))
       (ann (q 1) (P Integer))
       (ann (p 1) (p Integer))
       (ann (q 1) (q Integer))))