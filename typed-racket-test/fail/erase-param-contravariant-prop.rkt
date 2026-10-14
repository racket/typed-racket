#;
(exn-pred #rx"expected \\(-> Any Boolean : #:\\+ Bot")
#lang typed/racket

;; The values a parameter accepts are a contravariant (negative)
;; position, so a prop about x there must erase to Bot, not Top, when x
;; goes out of scope.

(define p
  (let ([x : Any ""])
    (define prm : (Parameterof (Any -> Boolean : #:+ (Number @ x)))
      (make-parameter (lambda ([w : Any]) (number? x))))
    (cons prm (lambda () (if ((prm) 0) (add1 x) 1)))))

((car p) (lambda ([w : Any]) #t))
((cdr p))
