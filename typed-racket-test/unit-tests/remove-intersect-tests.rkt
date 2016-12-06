#lang racket/base
(require "test-utils.rkt"
         (for-syntax racket/base)
         (r:infer infer)
         (rep type-rep)
         (types abbrev numeric-tower subtype subtract overlap)
         rackunit)
(provide tests)
(gen-test-main)

(define-syntax (over-tests stx)
  (syntax-case stx ()
    [(_ [t1 t2 res] ...)
     #'(test-suite "Tests for overlap"
                   (test-check (format "~a ~a" 't1 't2)
                               (lambda (a b) (eq? (not (not a)) b))
                               (overlap? t1 t2) res) ...)]))

(define overlap-tests
  (over-tests
   [-Number -Integer #t]))

(define-syntax (inter-tests stx)
  (syntax-case stx ()
    [(_ [t1 t2 res] ...)
     #'(test-suite "Tests for intersect"
                   (test-check (format "~a ~a" 't1 't2)
                               type-compare?
                               (intersect t1 t2) res) ...)]))


(define intersect-tests
  (inter-tests
   [-Number (Un -Number -Symbol) -Number]
   [-Number -Number -Number]
   [(Un (-val 'foo) (-val 6)) (Un -Number -Symbol) (Un (-val 'foo) (-val 6))]
   [-Number (-mu a (Un -Number -Symbol (make-Listof a))) -Number]
   [(Un -Number -Boolean) (-mu a (Un -Number -Symbol (make-Listof a))) -Number]
   [(-mu x (Un -Number (make-Listof x))) (Un -Symbol -Number -Boolean) -Number]
   [(Un -Number -String -Symbol -Boolean) -Number -Number]

   [(-lst -Number) (-pair Univ Univ) (-pair -Number (-lst -Number))]
   [(-lst -Number) (-poly (a) (-lst a)) (-poly (a) (-lst a))]
   ;; FIXME
   #;
   [-Listof -Sexp (-lst (Un B N -String Sym))]
   #;
   [-Sexp -Listof (-lst -Sexp)]
   [(-val "one") -Fixnum (Un)]
   [(Un (-val "one") (-val "two")) (Un (-val "one") (-val 1)) (-val "one")]
   ;; intersection cases
   [(-v a) -String (-unsafe-intersect (-v a) -String)]
   [-String (-v a) (-unsafe-intersect (-v a) -String)]
   [(-> -Number -Number) (-> -String -String) (-unsafe-intersect (-> -Number -Number)
                                                                 (-> -String -String))]
   [(-mu x (Un (Un -Number -String) (-pair -Number x)))
    (-mu x (Un (Un -Number -Symbol) (-pair -Number x)))
    (-mu x (Un -Number (-pair -Number x)))]
   [(make-Listof (-mu x (Un -String (-HT -String x))))
    (make-Listof -HashtableTop)
    (make-Listof (-HT -String (-mu x (Un -String (-HT -String x)))))]))

(define-syntax (remo-tests stx)
  (syntax-case stx ()
    [(_ [t1 t2 res] ...)
     (syntax/loc stx
       (test-suite "Tests for subtract"
                   (test-check (format "~a ~a" 't1 't2) type-compare? (subtract t1 t2) res) ...))]))

(define subtract-tests
  (remo-tests
   [(Un -Number -Symbol) -Number -Symbol]
   [-Number -Number (Un)]
   [(-mu x (Un -Number -Symbol (make-Listof x)))
    -Number
    (Un -Symbol (make-Listof (-mu x (Un -Number -Symbol (make-Listof x)))))]
   [(-mu x (Un -Number -Symbol -Boolean (make-Listof x)))
    -Number
    (Un -Symbol -Boolean (make-Listof (-mu x (Un -Number -Symbol -Boolean (make-Listof x)))))]
   [(Un (-val #f) (-mu x (Un -Number -Symbol (make-Listof (-v x)))))
    (Un -Boolean -Number)
    (Un -Symbol (make-Listof (-mu x (Un -Number -Symbol (make-Listof x)))))]
   [(Un (-val 'foo) (-val 6)) (Un -Number -Symbol) (Un)]
   [(-> (Un -Symbol -Number) -Number) (-> -Number -Number) (Un)]
   [(Un (-poly (a) (make-Listof a)) (-> -Number -Number))
    (-> -Number -Number)
    (-poly (a) (make-Listof a))]
   [(Un -Symbol -Number) (-poly (a) -Number) -Symbol]
   [(-pair -Number (-v a)) (-pair Univ Univ) (Un)]
   ))

(define tests
  (test-suite "Subtract Intersect"
     subtract-tests
     intersect-tests
     overlap-tests))
