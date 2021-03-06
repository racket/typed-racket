#lang racket/base

(require "test-utils.rkt"
         typed-racket/rep/type-rep
         typed-racket/types/utils
         typed-racket/types/abbrev
         typed-racket/types/numeric-tower
         typed-racket/types/substitute
         rackunit)
(provide tests)
(gen-test-main)

(define-syntax-rule (s img var tgt result)
  (test-equal? (format "~a" '(img tgt))
               (substitute img 'var tgt)
               result))


(define-syntax-rule (s* imgs rest var tgt result)
  (test-equal? (format "~a" '(img tgt))
               (substitute-dots (list . imgs) rest 'var tgt)
               result))

(define-syntax-rule (s... imgs var tgt result)
  (test-equal? (format "~a" '(img tgt))
               (substitute-dots (list . imgs) #f 'var tgt)
               result))

(define tests
  (test-suite "Tests for type substitution"
    (s -Number a (-v a) -Number)
    (s -Number a (-pair (-v a) -String) (-pair -Number -String))
    (s -Number a (-pair -String (-v a)) (-pair -String -Number))
    (s* (-Symbol -String) #f a (make-ListDots (-v a) 'a) (-lst* -Symbol -String))
    (s* (-Symbol -String) Univ a (make-ListDots (-v a) 'a) (-lst* -Symbol -String #:tail (-lst Univ)))
    (s... (-Number -Boolean) a (make-Fun (list (make-arr-dots null -Number (-v a) 'a))) (-Number -Boolean . -> . -Number))
    (s... (-Number -Boolean) a (make-Fun (list (make-arr-dots (list -String) -Number (-v a) 'a))) (-String -Number -Boolean . -> . -Number))
    (s... (-Number -Boolean) a (make-Fun (list (make-arr-dots (list -String) -Number (-v b) 'a))) (-String (-v b) (-v b) . -> . -Number))
    (s... (-Number -Boolean) a (make-Fun (list (make-arr-dots (list -String) -Number (-v b) 'b)))
          (make-Fun (list (make-arr-dots (list -String) -Number (-v b) 'b))))))
