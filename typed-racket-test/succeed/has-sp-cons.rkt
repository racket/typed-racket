#lang typed/racket

(: foo (Struct-Property Number))
(: foo? : Any -> Boolean : (Has-Struct-Property foo))
(: foo-ref : (Has-Struct-Property foo) -> Number)
(define-values (foo foo? foo-ref) (make-struct-type-property 'foo))

(: bar? : Any -> Boolean : (Has-Struct-Property foo))
(define bar? foo?)


;; (struct (X Y) helloworld ([x : Y] [y : Y]) #:property foo 20)
(struct helloworld ([x : Number] [y : Number]) #:property foo 20)

;;(struct (X Y) helloworld () #:property foo 20)


;; (define (process-sp-foo-occ [x : Any]) : Number
;;   (if (foo? x) (foo-ref x)
;;       42))

;; (define (process-sp-foo [x : (Has-Struct-Property foo)]) : Number
;;   (foo-ref x))

;; (process-sp-foo-occ (helloworld 10 11))
