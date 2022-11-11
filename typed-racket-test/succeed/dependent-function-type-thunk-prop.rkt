#lang typed/racket #:with-refinements

(: n? (-> ([v : Any]) (-> () Boolean #:+ (: v Number) #:- (! v Number))))
(define ((n? v)) (number? v))

(: f (-> Any Number))
(define (f x)
  (cond [((n? x)) (+ x 1)]
        [else 1]))

(ann (f 5) Number)
(ann (f #f) Number)
