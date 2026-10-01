#;
(exn-pred #rx"type mismatch")
#lang typed/racket

;; Same shape as erase-id-negative-prop.rkt, but x is bound by an
;; internal definition, which expands to letrec-values, so the erasure
;; happens when the letrec's clauses go out of scope. The prop
;; (Number @ x) in the (negative) domain position must erase to Bot.

(define g
  (let ()
    (define x : Any "")
    (lambda ([f : (Any -> Boolean : #:+ (Number @ x))])
      (if (f x) (add1 x) 1))))

(g (lambda ([w : Any]) #t))
