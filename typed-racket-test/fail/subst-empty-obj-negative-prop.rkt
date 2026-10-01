#;
(exn-pred #rx"type mismatch")
#lang typed/racket

;; Substituting the empty object for x must not erase the latent prop
;; (Number @ x) to Top when it occurs in a negative position (here, the
;; domain of the function type for f). Erasing it to Top let this
;; program typecheck and then crash at runtime with
;; (add1 ""), since (λ ([w : Any]) true) never establishes (Number @ x).

(((lambda ([x : Any])
    (lambda ([f : (Any -> Boolean : #:+ (Number @ x))])
      (if (f x) (add1 x) 1)))
  (ann "" Any))
 (λ ([w : Any]) true))
