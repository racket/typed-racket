#lang racket/base

(require
  rackunit
  racket/sandbox
  racket/flonum racket/fixnum racket/unsafe/ops
  racket/math
  syntax/srcloc
  (for-syntax
    racket/base
    syntax/parse))

(provide tests)

;; Test suite that makes sure that TR+opt returns the same results as Racket.
;; Historically contained bugs that we knew about, but had not fixed.
;; Now those are all fixed.

(define (mk-eval lang)
  (call-with-trusted-sandbox-configuration
   (λ ()
     (parameterize ([sandbox-memory-limit 300])
       (make-evaluator lang)))))
(define racket-eval (mk-eval 'racket))
(define tr-eval     (mk-eval 'typed/racket))

(define-syntax bad-opt
  (syntax-parser
    [(_ exp:expr)
     #`(test-case #,(format "~a" (syntax->datum #'exp))
         (define r-value (racket-eval #'exp))
         (define tr-value (tr-eval #'exp))
         (with-check-info* (list (make-check-info 'r-value r-value)
                                 (make-check-info 'tr-value tr-value)
                                 (make-check-location (build-source-location-list (quote-syntax exp))))
           (λ ()
             (when (equal? r-value tr-value)
               (fail-check "Known bug no longer exists.")))))]))

(define-syntax good-opt
  (syntax-parser
    [(_ exp:expr)
     #`(test-case #,(format "~a" (syntax->datum #'exp))
         (define r-value (racket-eval #'exp))
         (define tr-value (tr-eval #'exp))
         (with-check-info (['r-value r-value]
                           ['tr-value tr-value]
                           ['location (build-source-location-list (quote-syntax exp))])
           (when (not (equal? r-value tr-value))
             (fail-check "Optimizer regression"))))]))


(define tests
  (test-suite "Known bugs"

    ;; Arguments are converted to inexact too early
    (good-opt (* (make-rectangular -inf.0 1) (* 1 1)))
    (good-opt (/ -inf.0-inf.0i 8))
    (good-opt (- (* -1 1 +nan.0) 1.0+1.0i))
    (good-opt (- (* (/ 6 11) (/ 1.2345678f0 123456.7f0)) (make-rectangular 0.0 0.3)))
    (good-opt (/ 1.0 0.0+0.0i))
    (good-opt (+ 0.0+0.0i (* 1 1 +inf.0)))
    (good-opt (* 1.0f-30 1.0f-30 1.0e60+1.0e60i))

    ;; Unary division has bad underflow
    (good-opt (/ (make-rectangular 1e+100 1e-300)))
    (good-opt (/ 0.5+1.7e+308i))
    (good-opt (/ 1 (make-rectangular 1e+100 1e-300)))
    (good-opt (/ 1 0.5+1.7e+308i))

    ;; Division of complex 0 should only make part of the result nan
    (good-opt (/ 0.0+0.0i))
    (good-opt (/ 1 0.0+0.0i))
    (good-opt (/ 1.5 -3.0+9.8e-324i))

    ;; Division of complex infinity should only make part of the result nan
    (good-opt (/ (make-rectangular 1.0 +inf.0)))
    (good-opt (/ (make-rectangular +inf.0 1.0)))
    (good-opt (/ 1 (make-rectangular 1.0 +inf.0)))
    (good-opt (/ 1 (make-rectangular +inf.0 1.0)))

    ;; Exp of large real should have 0 imaginary component
    (good-opt (+ (exp 1.7976931348623151e+308) 0.0+0.0i))

    ;; Multiplication of multiple args should keep exact semantics for exact args
    (good-opt (* (expt 10 500) (expt 10 -500) 1.0+1.0i))

    ;; Addition of multiple args should keep exact semantics for exact args
    (good-opt (+ (expt 10 501) (expt -10 501) 1.0+1.0i))

    ;; Magnitude should not overflow unless necessary
    (good-opt (magnitude 3.0e300+4.0e300i))

    ;; Negation should correctly compute sign of 0.0
    (good-opt (- 0.0+0.0i))
    (good-opt (- 0+0i 0.0+0.0i))

    ;; Conjugate should correctly compute sign of 0.0
    (good-opt (conjugate 0.0+0.0i))

    ;; Magnitude should always return positive results
    (good-opt (magnitude -1.0-2i))))

(module+ main
  (require rackunit/text-ui)
  (void (run-tests tests)))
