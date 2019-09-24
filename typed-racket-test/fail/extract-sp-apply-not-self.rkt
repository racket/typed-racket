#;
(exn-pred "Type Checker: type mismatch")
#lang typed/racket

(: prop-ins-to-num (Struct-Property (-> Self Number)))
(: prop-ins-to-num? : Any -> Boolean : (Has-Struct-Property prop-ins-to-num))

(: prop-ins-to-num-ref (Exist (X) (-> (Has-Struct-Property prop-ins-to-num) (-> X Number) : X) ))
(define-values (prop-ins-to-num prop-ins-to-num? prop-ins-to-num-ref) (make-struct-type-property 'prop-ins-to-num))

;; (: bar? : Any -> Boolean : (Has-Struct-Property prop-ins-to-num))
;; (define bar? prop-ins-to-num?)


; (struct (X Y) helloworld ([x : Y] [y : Y]) #:property prop-ins-to-num (cons 20 40))

(struct posn ([x : Integer] [y : Integer])  #:property prop-ins-to-num (λ ([self : posn])
                                                                         20))

(: p1 posn)
(define p1 (posn 100 200))

(define (func [x : Any]) : Number
  (if (prop-ins-to-num? x)
      ((prop-ins-to-num-ref x) p1)
      42))

(func p1)

