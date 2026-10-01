#;
(exn-pred #rx"Wrong function argument type")
#lang typed/racket

;; Keyword argument types are a negative position too: when x goes out
;; of scope, the prop about it must erase to Bot.

(define g
  (let ([x (ann "" Any)])
    (lambda (#:f [f : (-> Any Boolean : #:+ (Number @ x))])
      (if (f x) (add1 x) 1))))

(g #:f (lambda ([w : Any]) #t))
