#lang typed/racket

(: prop-ins-to-num (Struct-Property (-> Self Number)))
(: prop-ins-to-num? : Any -> Boolean : (Has-Struct-Property prop-ins-to-num))

(: prop-ins-to-num-ref (Exist (X) (-> (Has-Struct-Property prop-ins-to-num) (-> X Number) : X) ))
(define-values (prop-ins-to-num prop-ins-to-num? prop-ins-to-num-ref) (make-struct-type-property 'prop-ins-to-num))

;; (: bar? : Any -> Boolean : (Has-Struct-Property prop-ins-to-num))
;; (define bar? prop-ins-to-num?)


(struct posn ([x : Integer] [y : Integer])  #:property prop-ins-to-num (λ ([self : posn])
                                                                         (posn-x self)))

(: p1 posn)
(define p1 (posn 100 200))

#; (: val Number)
(define val ((prop-ins-to-num-ref p1) p1))
(posn-x p1)


(define (func [x : Any]) : Number
  (if (prop-ins-to-num? x)
      ((prop-ins-to-num-ref x) x)
      42))

(func p1)

