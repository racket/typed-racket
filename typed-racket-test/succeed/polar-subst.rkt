#lang typed/racket

;; Companion to fail/subst-empty-obj-negative-prop.rkt and
;; fail/erase-id-negative-prop.rkt: polarity-aware erasure of the empty
;; object must not reject these sound uses of latent props.

;; a latent prop about a variable that stays in scope: nothing is
;; erased, and the argument genuinely establishes (Number @ x)
(define (f0 [x : Any]) : Number
  ((lambda ([f : (Any -> Boolean : #:+ (Number @ x))])
     (if (f x) (add1 x) 1))
   (lambda ([w : Any]) (number? x))))

;; positive erasure: props about an erased argument in result position
;; weaken to Top and the application still typechecks
(define (f1) : Boolean
  ((lambda ([y : Any]) (number? y)) "hello"))

;; the same, through let
(define f2
  (let ([x (ann 5 Any)])
    (lambda () (number? x))))

;; erasing a variable whose type already establishes the prop keeps the
;; prop true, even in a negative position
(define f3
  (let ([x : Number 5])
    (lambda ([f : (Any -> Boolean : #:+ (Number @ x))])
      (if (f x) (add1 x) 1))))

;; a box whose contents mention an erased variable widens to BoxTop,
;; which can still be read
(define b
  (let ([x : Any ""])
    (ann (box (lambda ([w : Any]) (number? x)))
         (Boxof (Any -> Boolean : #:+ (Number @ x))))))

;; the values a parameter produces are a positive position
(define prm
  (let ([x : Any ""])
    (ann (make-parameter (lambda ([w : Any]) (number? x)))
         (Parameterof (Any -> Boolean : #:+ (Number @ x))))))

(f0 1)
(f1)
(f2)
(f3 (lambda ([w : Any]) #t))
(unbox (ann b BoxTop))
((prm) 1)
