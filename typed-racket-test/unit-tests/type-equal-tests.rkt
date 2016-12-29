#lang racket/base

(require "test-utils.rkt" (for-syntax racket/base)
         (rep type-rep)
         (types abbrev numeric-tower)
         rackunit)

(provide tests)
(gen-test-main)

(define (-opaque x) (make-Opaque x))


(define-syntax (te-tests stx)
  (define (single-test stx)
    (syntax-case stx (FAIL)
      [(FAIL t s) (syntax/loc stx (test-check (format "FAIL ~a" '(t s))
                                    (lambda (a b) (not (equal? a b))) t s))]
      [(t s) (syntax/loc stx (test-check (format "~a" '(t s)) equal? t s))]))
  (syntax-case stx ()
    [(_ cl ...)
     #`(test-suite "Tests for type equality"
         #,@(map single-test (syntax->list #'(cl ...))))]))

(define (fld* t) (make-fld t (datum->syntax #'here 'values) #f))

(define tests
  (te-tests
   [-Number -Number]
   [(Un -Number) -Number]
   [(Un -Number -Symbol -Boolean) (Un -Number -Boolean -Symbol)]
   [(Un -Number -Symbol -Boolean) (Un -Symbol -Boolean -Number)]
   [(Un -Number -Symbol -Boolean) (Un -Symbol -Number -Boolean)]
   [(Un -Number -Symbol -Boolean) (Un -Boolean (Un -Symbol -Number))]
   [(Un -Number -Symbol) (Un -Symbol -Number)]
   [(-poly (x) (-> (Un -Symbol -Number) x)) (-poly (xyz) (-> (Un -Number -Symbol) xyz))]
   [(-mu x (Un -Number -Symbol x)) (-mu y (Un -Number -Symbol y))]
   ;; found bug
   [FAIL (Un (-mu heap-node
                  (-struct #'heap-node #f
                           (map fld* (list (-opaque #'comparator)
                                           -Number
                                           (-v a)
                                           (Un heap-node (-opaque #'heap-empty))))))
             (-opaque #'heap-empty))
         (Un (-mu heap-node
                  (-struct #'heap-node #f
                           (map fld* (list (-opaque #'comparator)
                                           -Number
                                           (-pair -Number -Number)
                                           (Un heap-node (-opaque #'heap-empty))))))
             (-opaque #'heap-empty))]))
