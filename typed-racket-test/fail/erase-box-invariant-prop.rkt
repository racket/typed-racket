#;
(exn-pred #rx"set-box!")
#lang typed/racket

;; A prop about x underneath Boxof, which is invariant, can be neither
;; weakened nor strengthened when x goes out of scope. Erasing it to Top
;; (as a positive position would) let the box escape with type
;; (Boxof (-> Any Boolean)), so a function that never establishes
;; (Number @ x) could be stored into it and then trusted by the closure
;; that still has x in scope. The box's type must widen to BoxTop.

(define p
  (let ([x : Any ""])
    (define b : (Boxof (Any -> Boolean : #:+ (Number @ x)))
      (box (lambda ([w : Any]) (number? x))))
    (cons b (lambda () (if ((unbox b) 0) (add1 x) 1)))))

(set-box! (car p) (lambda ([w : Any]) #t))
((cdr p))
