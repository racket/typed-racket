#;
(exn-pred #rx"expected: \\(-> Any Boolean : #:\\+ Bot")
#lang typed/racket #:with-refinements

;; A linear inequality about x in a negative position must erase to Bot
;; when x goes out of scope; erasing it to Top accepted the argument
;; below and let the body conclude (<= x 0) for x = 5.

(define g
  (let ([x : Integer 5])
    (lambda ([f : (-> ([w : Any]) Boolean #:+ (<= x 0))])
      (if (f 0) (ann x (Refine [n : Integer] (<= n 0))) 0))))

(g (lambda ([w : Any]) #t))
