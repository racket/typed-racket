#;
(exn-pred #rx"expected: \\(-> Any Any : #:\\+ Bot #:- Bot")
#lang typed/racket

;; A function type in a negative position that promises to return x
;; (#:object x) is an obligation; when x goes out of scope the
;; obligation can no longer be stated, so no argument may satisfy it.
;; Dropping the object instead let any function through, after which
;; (number? (f 0)) wrongly refined x.

(define g
  (let ([x (ann "" Any)])
    (lambda ([f : (-> ([w : Any]) Any #:object x)])
      (if (number? (f 0)) (add1 x) 1))))

(g (lambda ([w : Any]) 3))
