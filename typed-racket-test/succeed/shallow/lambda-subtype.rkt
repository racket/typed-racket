#lang typed/racket/base/shallow

;; shallow domain checks must be based off the function's formals --- not
;;  on the type assigned to the whole expression
;;
;; because the function body might assume (-> T0 T1)
;; and the "whole" type might be (-> T2 T1) where T2 <: T0
;; and if that happens, the shallow check could incorrectly error on some
;; T0 inputs
;;
;; discovered in the quadT & synth benchmarks

;; ---

(define-type Quad Symbol)
(define-type LineQuad Symbol)
(define-type Wrap-Proc-Type (((Listof Quad)) (Float) . ->* . (Listof LineQuad)))

(: quadT-fun Wrap-Proc-Type)
(define (quadT-fun q* [n #f])
  (if n
    (list (string->symbol (number->string n)))
    q*))

(quadT-fun '())

;; ---

(define-type Array (Vectorof Real))

(: synth-array-append* (-> (Listof Array) (Listof Array) Array))
(define (synth-array-append* aa bb [k 0])
  (vector k))

(synth-array-append* '() '())
