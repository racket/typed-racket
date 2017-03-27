#lang racket/base

(require "test-utils.rkt"
         rackunit racket/format
         (rep prop-rep)
         (types abbrev prop-ops)
         (for-syntax racket/base syntax/parse))

(provide tests)
(gen-test-main)




(define (not-atomic-implies? y x) (not (atomic-implies? y x)))
(define (not-implies? y x) (not (implies? y x)))

;; binding these means props that mention #'x, #'y, and #'z
;; will be referring to identifiers w/ variable bindings
;; (and props about things w/o variable bindings are erased
;; since top level ids are mutable)
(define x 42)
(define y 42)
(define z 42)

(define-syntax (implies-atomic-tests stx)
  (define-syntax-class imp-test
    (pattern (~and tst [#:not a:expr c:expr])
             #:with check #`(test-case
                             (~a '(not (implies-atomic? a c)))
                             #,(syntax/loc #'tst (check-false (atomic-implies? a c)))
                             #,(syntax/loc #'tst (check-false (implies? a c)))))
    (pattern (~and tst [a:expr c:expr])
             #:with check #`(test-case
                             (~a '(implies-atomic? a c))
                             #,(syntax/loc #'tst (check-true (atomic-implies? a c)))
                             #,(syntax/loc #'tst (check-true (implies? a c))))))
  (syntax-parse stx
    [(_ tst:imp-test ...)
     #'(test-suite
        "Implies Atomic"
        tst.check ...)]))

(define-syntax (test-opposite stx)
  (define-syntax-class complementary
     (pattern #:complementary #:with check #'check-true)
     (pattern #:not-complementary #:with check #'check-false))
  (define-syntax-class contradictory
     (pattern #:contradictory #:with check #'check-true)
     (pattern #:not-contradictory #:with check #'check-false))
  (syntax-parse stx
    [(_ comp:complementary contr:contradictory p1* p2*)
     #`(test-case
        (~a '(opposite p1* p2*))
        (define p1 p1*)
        (define p2 p2*)
        #,(syntax/loc stx (comp.check (atomic-complement? p1 p2) "Complementary"))
        #,(syntax/loc stx (contr.check (atomic-contradiction? p1 p2) "Contradictory"))
        ;; 'contradiction' should be strictly stronger than 'atomic-contradiction'
        ;; and so we'll test both here
        #,(syntax/loc stx (contr.check (contradiction? p1 p2) "Contradictory")))]))


(define tests
  (test-suite "Props"
    (test-suite "Opposite"
      (test-opposite #:not-complementary #:contradictory
        (-is-type 0 -Symbol)
        (-not-type 0 (Un -Symbol -String)))

      (test-opposite #:complementary #:not-contradictory
        (-is-type 0 (Un -Symbol -String))
        (-not-type 0 -Symbol))

      (test-opposite #:complementary #:contradictory
        (-not-type 0 -Symbol)
        (-is-type 0 -Symbol))

      (test-opposite #:not-complementary #:not-contradictory
        (-is-type 1 -Symbol)
        (-not-type 0 -Symbol))

      (test-opposite #:not-complementary #:not-contradictory
        (-not-type 0 -Symbol)
        (-is-type 0 -String))

      (test-opposite #:not-complementary #:not-contradictory
        (-not-type 0 -Symbol)
        (-is-type 0 -String))

      (test-opposite #:not-complementary #:contradictory
        -ff
        -ff)

      (test-opposite #:complementary #:contradictory
        -ff
        -tt)

      (test-opposite #:complementary #:not-contradictory
        -tt
        -tt)

      (test-opposite #:not-complementary #:not-contradictory
        (-leq (-lexp (list 2 (-id-path #'x))) (-id-path #'x))
        (-leq (-lexp (list 2 (-id-path #'x))) (-id-path #'x)))

      (test-opposite #:complementary #:not-contradictory
        (-leq (-lexp (list 2 (-id-path #'x))) (-id-path #'x))
        (-leq (-id-path #'x) (-lexp (list 2 (-id-path #'x)))))

      (test-opposite #:not-complementary #:contradictory
        (-leq (-id-path #'x) (-lexp 42))
        (-leq (-lexp 100) (-id-path #'x)))

      (test-opposite #:complementary #:contradictory
        (-leq (-lexp (list 2 (-id-path #'x))) (-id-path #'x))
        (negate-prop (-leq (-lexp (list 2 (-id-path #'x))) (-id-path #'x))))

    )

    (test-suite
     "Contradiction"
     (check contradiction?
            (-is-type #'x (-pair -String -String))
            (-is-type (-car-of (-id-path #'x)) -Symbol))
     (check contradiction?
            (-is-type (-car-of (-id-path #'x)) (-pair -String -String))
            (-is-type (-cdr-of (-car-of (-id-path #'x))) -Symbol))
     (check contradiction?
            (-is-type (-car-of (-id-path #'x)) -Symbol)
            (-is-type #'x (-pair -String -String)))
     (check contradiction?
            (-is-type (-cdr-of (-car-of (-id-path #'x))) -Symbol)
            (-is-type (-car-of (-id-path #'x)) (-pair -String -String)))
     (check contradiction?
            (-not-type (-car-of (-id-path #'x)) -String)
            (-is-type #'x (-pair -String -String)))
     (check contradiction?
            (-is-type #'x (-pair -String -String))
            (-not-type (-car-of (-id-path #'x)) -String))
     (check contradiction?
            (-not-type (-car-of (-cdr-of (-id-path #'x))) -String)
            (-is-type (-cdr-of (-id-path #'x)) (-pair -String -String)))
     (check contradiction?
            (-is-type (-cdr-of (-id-path #'x)) (-pair -String -String))
            (-not-type (-car-of (-cdr-of (-id-path #'x))) -String))
     (check contradiction?
            (-leq (-id-path #'x) (-lexp 0))
            (-leq (-lexp 1) (-id-path #'x))))

    (implies-atomic-tests
     [-tt -tt]
     [-ff -ff]
     [#:not -tt -ff]
     [(-is-type 0 -Symbol) -tt]
     [-ff (-is-type 0 -Symbol)]
     [(-is-type 0 -Symbol)
      (-is-type 0 (Un -String -Symbol))]
     [#:not
      (-is-type 0 (Un -String -Symbol))
      (-is-type 0 -Symbol)]
     [(-not-type 0 (Un -String -Symbol))
      (-not-type 0 -Symbol)]
     [#:not
      (-not-type 0 -Symbol)
      (-not-type 0 (Un -String -Symbol))]
     [#:not
      (-is-type 0 -Symbol)
      (-is-type 1 -Symbol)]
     [(-is-type #'x -Symbol)
      (-is-type #'x -Symbol)]
     [(-is-type #'x -Symbol)
      (-or (-is-type 1 -Symbol) (-is-type #'x -Symbol))]
     [(-and (-is-type 1 -Symbol) (-is-type #'x -Symbol))
      (-is-type #'x -Symbol)]
     [(-is-type #'x -Symbol)
      (-not-type #'x (-val #f))]
     [(-leq (-id-path #'x) (-lexp 42))
      (-leq (-id-path #'x) (-lexp 50))]
     [#:not
      (-leq (-id-path #'x) (-lexp 50))
      (-leq (-id-path #'x) (-lexp 42))])


    (test-suite "Simplification"
      (check-equal?
        (-and (-is-type #'x -Symbol) (-not-type #'x (-val #f)))
        (-is-type #'x -Symbol))
      (check-equal?
        (-and (-not-type #'x (-val #f)) (-is-type #'x -Symbol))
        (-is-type #'x -Symbol))

      (check-equal?
        (-and (-is-type #'y (-val #f))
              (-or (-is-type #'y (-val #f))
                   (-is-type #'x (-val #f))))
        (-is-type #'y (-val #f)))

      (check-equal?
        (-and (-not-type #'y (-val #f))
              (-or (-not-type #'y (-val #f))
                   (-not-type #'x (-val #f))))
        (-not-type #'y (-val #f)))

      (check-equal?
       (-or (-not-type #'y (-val #f))
            (-leq (-id-path #'x) (-lexp 42))
            (-leq (-id-path #'x) (-lexp 50)))
       (-or (-not-type #'y (-val #f))
            (-leq (-id-path #'x) (-lexp 50))))

      (check-equal?
       (-and (-is-type (-id-path #'z) (-val #f))
             (-or (-not-type #'y (-val #f))
                  (-leq (-lexp 50) (-id-path #'x))
                  (-leq (-id-path #'x) (-lexp 50))))
       (-is-type (-id-path #'z) (-val #f))))

  ))
