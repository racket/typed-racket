#;
(exn-pred #rx"type mismatch")
#lang typed/racket

;; Same shape as subst-empty-obj-negative-prop.rkt, but the variable the
;; latent prop speaks about goes out of scope via let, so the erasure
;; happens through erase-identifiers rather than object substitution at
;; an application. The prop (Number @ x) in the (negative) domain
;; position must erase to Bot, not Top.

(define g
  (let ([x (ann "" Any)])
    (lambda ([f : (Any -> Boolean : #:+ (Number @ x))])
      (if (f x) (add1 x) 1))))

(g (lambda ([w : Any]) #t))
