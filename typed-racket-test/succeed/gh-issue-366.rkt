#lang typed/racket

;; Test for Github issue #366

(struct Parent () #:transparent)
(struct (A B) S0 Parent ([a : A] [b : B]) #:transparent)
(struct (B C) S1 Parent ([b : B] [c : C]) #:transparent)

(define v : (S1 'x Integer) (S1 'x 2))
(ann (if (S0? v)
         (S0-b v) ;; ERROR HERE
         (if (S1? v)
             (S1-b v)
             (void)))
     'x)
