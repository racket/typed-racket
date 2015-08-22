#lang racket/base

(provide u* u^ u*/F u^/F)

(require racket/match
         racket/hash
         "../../rep/measure-unit-rep.rkt")

(define u*
  (case-lambda
    [() (make-measure-unit (hash))]
    [(u1 . rst)
     (match-define (measure-unit: ht1) u1)
     (match-define (list (measure-unit: rst-hts) ...) rst)
     (make-measure-unit (apply hash-union ht1 rst-hts #:combine +))]))

(define (u^ u1 n)
  (match-define (measure-unit: ht) u1)
  (make-measure-unit
   (for/hash ([(k v) (in-hash ht)])
     (define v*n (* v n))
     (unless (integer? v*n)
       (error 'u^ "non-integer result exponent: ~v" v*n))
     (values k v*n))))

(define (u*/F . args)
  (apply/F-measure-units u* args))

(define (u^/F u1 n)
  (app/F-measure-units u^ u1 n))
