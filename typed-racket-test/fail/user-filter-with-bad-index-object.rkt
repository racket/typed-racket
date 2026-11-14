#;
(exn-pred #rx"index \\(0 3\\) refers to argument 3, but the function type has only 1 argument")
#lang typed/racket

;; This test ensures that a filter object like '3' is
;; invalid when the function type only has 1 argument.

(ann (λ (x)
       (define f
         (ann (λ (y) (exact-integer? x))
              (Any -> Boolean : #:+ (Integer @ 3) #:- (! Integer @ x))))
       (if (f 'dummy)
           (add1 x)
           2))
     (Any -> Integer))

