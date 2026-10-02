#lang typed/racket

(: double-num? (-> ([x : Any])
                   (-> ([y : Any]) Boolean #:+ (: x Number))))
(define ((double-num? x) y) (number? x))

(: double-num?/pos (-> Any (-> Any Boolean : #:+ (: (1 0) Number))))
(define ((double-num?/pos x) y) (number? x))

(: use-named (-> Any Any Number))
(define (use-named x y)
  (if ((double-num? x) y)
      (+ x 1)
      0))
