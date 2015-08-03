#lang typed/racket

(define-signature is-3x-sig ([is-3x : (-> Natural Boolean)]))
(define-signature is-3x+1-sig ([is-3x+1 : (-> Natural Boolean)]))
(define-signature is-3x+2-sig ([is-3x+2 : (-> Natural Boolean)]))

(define is-3x-unit
  (unit 
    (import is-3x+2-sig)
    (export is-3x-sig)
    (: is-3x (-> Natural Boolean))
    (define (is-3x x)
      (or (= 0 x) (is-3x+2 (sub1 x))))))

(define is-3x+2-unit
  (unit 
    (import is-3x+1-sig)
    (export is-3x+2-sig)
    (: is-3x+2 (-> Natural Boolean))
    (define (is-3x+2 x)
      (and (> x 0) (is-3x+1 (sub1 x))))))

(define is-3x+1-unit
  (unit
    (import is-3x-sig)
    (export is-3x+1-sig)
    (: is-3x+1 (-> Natural Boolean))
    (define (is-3x+1 x)
      (and (> x 0) (is-3x (sub1 x))))))

(define 3x-compound1
  (compound-unit (import (IS-3X : is-3x-sig))
                 (export IS-3X+1 IS-3X+2)
                 (link (((IS-3X+1 : is-3x+1-sig)) is-3x+1-unit IS-3X)
                       (((IS-3X+2 : is-3x+2-sig)) is-3x+2-unit IS-3X+1))))

(define 3x-compound2
  (compound-unit (import)
                 (export IS-3X)
                 (link (((IS-3X : is-3x-sig)) is-3x-unit IS-3X+2)
                       (((IS-3X+1 : is-3x+1-sig)
                         (IS-3X+2 : is-3x+2-sig)) 3x-compound1 IS-3X))))

(define 3x-run-unit
  (unit 
    (import is-3x-sig is-3x+1-sig is-3x+2-sig)
    (export)
    (list (is-3x 1)
          (is-3x 3)
          (is-3x+1 5)
          (is-3x+1 7)
          (is-3x+2 4)
          (is-3x+2 8))))

(define 3x-compound3
  (compound-unit (import)
                 (export IS-3X IS-3X+1 IS-3X+2)
                 (link (((IS-3X : is-3x-sig)) 3x-compound2)
                       (((IS-3X+1 : is-3x+1-sig)
                         (IS-3X+2 : is-3x+2-sig)) 3x-compound1 IS-3X)
                       (() 3x-run-unit IS-3X IS-3X+1 IS-3X+2))))

(invoke-unit 3x-compound3)

(define-values/invoke-unit 3x-compound3 (import) (export is-3x-sig is-3x+1-sig is-3x+2-sig))

(list (is-3x+2 8)
      (is-3x+1 7)
      (is-3x 6))


