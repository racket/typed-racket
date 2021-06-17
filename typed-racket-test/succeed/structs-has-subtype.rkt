#lang typed/racket

(: prop-ins-to-num (Struct-Property (-> Self Number)))
(: prop-ins-to-num? : Any -> Boolean : (Has-Struct-Property prop-ins-to-num))

(: prop-ins-to-num-ref (-> (Has-Struct-Property prop-ins-to-num) (Some (X) (-> X Number) : #:+ X)))
(define-values (prop-ins-to-num prop-ins-to-num? prop-ins-to-num-ref) (make-struct-type-property 'prop-ins-to-num))


(struct posn ([x : Integer] [y : Integer])
  #:property prop-ins-to-num
  (λ ([self : posn])
    20))

(: p1 posn)
(define p1 (posn 100 200))
(posn-x p1)

(prop-ins-to-num-ref struct:posn)
(: val Number)
(define val ((prop-ins-to-num-ref p1) p1))
