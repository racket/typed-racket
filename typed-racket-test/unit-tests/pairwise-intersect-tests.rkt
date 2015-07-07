#lang racket
(require "test-utils.rkt"
         rackunit
         typed-racket/utils/utils
         (types pairwise-intersect abbrev union numeric-tower)
         (rep type-rep prop-rep))

(provide tests)
(gen-test-main)

(define-syntax (check-intersect stx)
  (syntax-case stx ()
    [(_ (s t) result)
     (syntax/loc stx
       (check type-equal? (pairwise-intersect s t) result))]))
(define intersect-exn? #rx"pairwise-intersect[^:]*:")
(define-syntax (check-intersect/err stx)
  (syntax-case stx ()
    [(_ (s t))
     (syntax/loc stx
       (check-exn intersect-exn? (λ () (pairwise-intersect s t))))]))

(define tests
  (test-suite
   "Pairwise intersection tests"
   (check-intersect (Univ -Integer)
                    -Integer)
   (check-intersect (-Integer -String)
                    (Un))
   (check-intersect ((-pair Univ -String) (-pair -Integer Univ))
                    (-pair -Integer -String))
   (check-intersect ((-> Univ -Integer) (-> -Integer Univ))
                    (-> -Integer -Integer))
   (check-intersect ((->key Univ #:x -Integer #t -Integer)
                     (->key Univ #:x -PosReal #t -Integer))
                    (->key Univ #:x -PosInt #t -Integer))
   (check-intersect/err ((->key Univ #:x -Integer #t -Integer)
                         (->key Univ #:x -PosReal #f -Integer)))
   (check-intersect ((-> Univ -Boolean)
                     (-> Univ -Boolean : (-PS (-is-type 0 -Integer) (-not-type 0 -Integer))))
                    (-> Univ -Boolean : (-PS (-is-type 0 -Integer) (-not-type 0 -Integer))))
   (check-intersect ((-poly (a) (-> a a)) (-> -Integer -Integer))
                    (-poly (a) (-> a a)))
   (check-intersect ((-> Univ Univ) (-poly (a) (-> a a)))
                    (-poly (a) (-> a a)))
   ))
