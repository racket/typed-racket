#lang racket/base

(require "test-utils.rkt"
         (rep type-rep object-rep filter-rep)
         (types utils abbrev numeric-tower substitute filter-ops)
         (typecheck tc-subst)
         racket/match rackunit)
(provide tests)
(gen-test-main)

(define-syntax-rule (id-subst-obj o var new-o result)
  (test-true (format  "~a" '(o [new-o / var])) (let ([o* (subst-object o #'var new-o #t)])
                                        (or (object-equal? o* result)
                                            (list 'expected: result 'actual: o*)))))

(define-syntax-rule (id-subst-SLI slis var new-o result)
  (test-true (format  "~a" '(slis [new-o / var])) (let ([expected (apply -and result)]
                                            [actual (subst-filter (apply -and slis) #'var new-o #t)])
                                        (or (filter-equal? actual expected)
                                            (list 'expected: expected 'actual: actual)))))

(define-syntax-rule (id-subst-many-SLI slis symbols new-os result)
  (test-true (format  "~a" '(slis [new-os / symbols])) (let ([expected (apply -and result)]
                                                             [actual (for/fold ([filter (apply -and slis)])
                                                                               ([sym (in-list symbols)]
                                                                                [new-o (in-list new-os)])
                                                                       (subst-filter filter (datum->syntax #f sym) new-o #t))])
                                                        (or (filter-equal? actual expected)
                                                            (list 'expected: expected 'actual: actual)))))

(define-syntax-rule (s img var tgt result)
  (test-eq? (format "~a" '(img tgt)) (substitute img 'var tgt) result))

(define-syntax-rule (s* imgs rest var tgt result)
  (test-eq? (format "~a" '(img tgt)) (substitute-dots (list . imgs) rest 'var tgt) result))

(define-syntax-rule (s... imgs var tgt result)
  (test-eq? (format "~a" '(img tgt)) (substitute-dots (list . imgs) #f 'var tgt) result))

(define tests
  (test-suite 
   "Tests for substitution"
   (s -Number a (-v a) -Number)
   (s* (-Symbol -String) #f a (make-ListDots (-v a) 'a) (-lst* -Symbol -String))
   (s* (-Symbol -String) Univ a (make-ListDots (-v a) 'a) (-lst* -Symbol -String #:tail (-lst Univ)))
   (s... (-Number -Boolean) a (make-Function (list (make-arr-dots null -Number (-v a) 'a))) (-Number -Boolean . -> . -Number))
   (s... (-Number -Boolean) a (make-Function (list (make-arr-dots (list -String) -Number (-v a) 'a))) (-String -Number -Boolean . -> . -Number))
   (s... (-Number -Boolean) a (make-Function (list (make-arr-dots (list -String) -Number (-v b) 'a))) (-String (-v b) (-v b) . -> . -Number))
   (s... (-Number -Boolean) a (make-Function (list (make-arr-dots (list -String) -Number (-v b) 'b)))
         (make-Function (list (make-arr-dots (list -String) -Number (-v b) 'b))))
   
   ;; LExp/SLI tests
   (id-subst-obj (-id-lexp (1 x)) x (-id-path #'y) (-id-lexp (1 y)))
   (id-subst-obj (-id-lexp 42 (2 x) (3 z)) x (-id-path #'y) (-id-lexp 42 (2 y) (3 z)))
   (id-subst-obj (-id-lexp 1 (2 x) (3 z)) x (-id-lexp (2 y)) (-id-lexp 1 (4 y) (3 z)))
   (id-subst-obj (-id-lexp 1 (2 x) (1 y)) x (-id-lexp 10 (2 y)) (-id-lexp 21 (5 y)))
   
   ;; simple replace y -> q
   (id-subst-SLI (-sli (-leq (-id-lexp (2 x))
                             (-id-lexp (3 y)))
                       (-leq (-id-lexp (4 y))
                             (-id-lexp (5 z))))
                 y 
                 (-id-path #'q)
                 (-sli (-leq (-id-lexp (2 x))
                             (-id-lexp (3 q)))
                       (-leq (-id-lexp (4 q))
                             (-id-lexp (5 z)))))
   
   ;; eliminate y
   (id-subst-SLI (-sli (-leq (-id-lexp (2 x))
                             (-id-lexp (3 y)))
                       (-leq (-id-lexp (4 y))
                             (-id-lexp (5 z)))) ;; 2x <= 3y,, 4y <= 5z --> 8x
                 y 
                 -empty-obj
                 (-sli (-leq (-id-lexp (8 x))
                             (-id-lexp (15 z)))))
   
   ;; eliminate y
   (id-subst-SLI (-sli (-leq (-id-lexp (2 x))
                             (-id-lexp (3 y)))
                       (-leq (-id-lexp (4 a))
                             (-id-lexp (5 z)))) ;; 2x <= 3y,, 4y <= 5z --> 8x
                 y 
                 -empty-obj
                 (-sli (-leq (-id-lexp (4 a))
                             (-id-lexp (5 z)))))
   
   
   ;; start w/ 2 disjoint SLIs, sub to make them join, then elim
   (id-subst-many-SLI (-sli (-leq (-id-lexp (1 x))
                                  (-id-lexp (2 y)))
                            (-leq (-id-lexp (3 q))
                                  (-id-lexp (4 z))))
                      '(y q r)
                      (list (-id-path #'r) (-id-path #'r) -empty-obj) 
                      (-sli (-leq (-id-lexp (3 x))
                                  (-id-lexp (8 z)))))))

