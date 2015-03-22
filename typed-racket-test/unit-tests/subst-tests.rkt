#lang racket/base

(require "test-utils.rkt"
         (rep type-rep object-rep)
         (types utils abbrev numeric-tower substitute)
         (typecheck tc-subst)
         racket/match rackunit)
(provide tests)
(gen-test-main)

(define-syntax-rule (id-sub-obj img var tgt result)
  (test-true (format "~a" '(img tgt)) (let ([o* (subst-object img #'var tgt #t)])
                                        (or (object-equal? o* result)
                                            (list 'expected: result 'actual: o*)))))

(define-syntax-rule (s img var tgt result)
  (test-eq? (format "~a" '(img tgt)) (substitute img 'var tgt) result))

(define-syntax-rule (s* imgs rest var tgt result)
  (test-eq? (format "~a" '(img tgt)) (substitute-dots (list . imgs) rest 'var tgt) result))

(define-syntax-rule (s... imgs var tgt result)
  (test-eq? (format "~a" '(img tgt)) (substitute-dots (list . imgs) #f 'var tgt) result))

(define tests
  (test-suite "Tests for substitution"
    (s -Number a (-v a) -Number)
    (s* (-Symbol -String) #f a (make-ListDots (-v a) 'a) (-lst* -Symbol -String))
    (s* (-Symbol -String) Univ a (make-ListDots (-v a) 'a) (-lst* -Symbol -String #:tail (-lst Univ)))
    (s... (-Number -Boolean) a (make-Function (list (make-arr-dots null -Number (-v a) 'a))) (-Number -Boolean . -> . -Number))
    (s... (-Number -Boolean) a (make-Function (list (make-arr-dots (list -String) -Number (-v a) 'a))) (-String -Number -Boolean . -> . -Number))
    (s... (-Number -Boolean) a (make-Function (list (make-arr-dots (list -String) -Number (-v b) 'a))) (-String (-v b) (-v b) . -> . -Number))
    (s... (-Number -Boolean) a (make-Function (list (make-arr-dots (list -String) -Number (-v b) 'b)))
          (make-Function (list (make-arr-dots (list -String) -Number (-v b) 'b))))
    
    ;; LExp/SLI tests
    (id-sub-obj (-id-lexp (1 x)) x (-id-path #'y) (-id-lexp (1 y)))
    (id-sub-obj (-id-lexp 42 (2 x) (3 z)) x (-id-path #'y) (-id-lexp 42 (2 y) (3 z)))
    (id-sub-obj (-id-lexp 1 (2 x) (3 z)) x (-id-lexp (2 y)) (-id-lexp 1 (4 y) (3 z)))
    (id-sub-obj (-id-lexp 1 (2 x) (1 y)) x (-id-lexp 10 (2 y)) (-id-lexp 21 (5 y)))))
