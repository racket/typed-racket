#lang racket/base

(require "test-utils.rkt"
         rackunit racket/format
         (typecheck tc-metafunctions tc-subst)
         (rep prop-rep type-rep object-rep values-rep)
         (types abbrev prop-ops tc-result numeric-tower)
         (for-syntax racket/base syntax/parse))

(provide tests)
(gen-test-main)

(define-syntax (test-combine-props stx)
  (syntax-parse stx
    [(_ new:expr existing:expr expected:expr box-v:expr)
     (quasisyntax/loc stx
       (test-case (~a '(new + existing = expected))
         (define success
           (let-values ([(res-formulas res-props) (combine-props new existing)])
             #,(syntax/loc stx (check-equal? (and res-formulas
                                                  (append res-formulas res-props))
                                             expected))
             #t))
         #,(syntax/loc stx (check-equal? success box-v))))]))

;; we create some bindings so that syntax for the unit tests
;; refers to identifiers that produce #t for identifier-binding
(define x 42)
(define y 42)
(define z 42)

(define tests
  (test-suite "Metafunctions"

    (test-suite "combine-props"

      (test-combine-props
        (list (-or (-not-type #'x -String) (-not-type #'y -String)))
        (list (-is-type #'x (Un -String -Symbol)) (-is-type #'y (Un -String -Symbol)))
        (list (-or (-not-type #'y -String) (-not-type #'x -String))
              (-is-type #'y (Un -String -Symbol)) (-is-type #'x (Un -String -Symbol)))
        #t)

      (test-combine-props
       (list (-or (-is-type #'x -String) (-is-type #'y -String)))
       (list (-is-type #'x (Un -String -Symbol)) (-is-type #'y (Un -String -Symbol)))
       (list (-or (-is-type #'y -String) (-is-type #'x -String))
             (-is-type #'y (Un -String -Symbol)) (-is-type #'x (Un -String -Symbol)))
       #t)

      (test-combine-props
       (list (-is-type (-car-of (-id-path #'y)) -Int))
       (list (-or (-is-type #'x (-pair -Int -Int))
                  (-is-type #'y (-pair -String -String))))
       (list (-is-type #'x (-pair -Int -Int))
             (-is-type (-car-of (-id-path #'y)) -Int))
       #t)

      (test-combine-props
       (list (-is-type (-car-of (-id-path #'y)) -Int)
             (-is-type (-car-of (-id-path #'x)) -String))
       (list (-or (-is-type #'x (-pair -Int -Int))
                  (-is-type #'y (-pair -String -String))))
       #f
       #t)

      (test-combine-props
       (list (-leq (-lexp 1 (-id-path #'x))
                   (-lexp (-id-path #'y)))
             (-is-type (-car-of (-id-path #'x)) -String))
       (list (-or (-is-type #'x (-pair -Int -Int))
                  (-leq (-lexp 1 (-id-path #'y))
                        (-lexp (-id-path #'x)))))
       #f
       #t)
    )

    (test-suite "merge-tc-results"
      (check-equal?
        (merge-tc-results (list))
        (ret -Bottom))
      (check-equal?
        (merge-tc-results (list (ret Univ)))
        (ret Univ))
      (check-equal?
        (merge-tc-results (list (ret Univ -tt-propset (make-Path null #'x))))
        (ret Univ -tt-propset (make-Path null #'x)))
      (check-equal?
        (merge-tc-results (list (ret -Bottom) (ret -Symbol -tt-propset (make-Path null #'x))))
        (ret -Symbol -tt-propset (make-Path null #'x)))
      (check-equal?
        (merge-tc-results (list (ret -String) (ret -Symbol)))
        (ret (Un -Symbol -String)))
      (check-equal?
        (merge-tc-results (list (ret -String -true-propset) (ret -Symbol -true-propset)))
        (ret (Un -Symbol -String) -true-propset))
      (check-equal?
        (merge-tc-results (list (ret (-val #f) -false-propset) (ret -Symbol -true-propset)))
        (ret (Un -Symbol (-val #f)) -tt-propset))
      (check-equal?
        (merge-tc-results (list (ret (list (-val 0) (-val 1))) (ret (list (-val 1) (-val 2)))))
        (ret (list (Un (-val 0) (-val 1)) (Un (-val 1) (-val 2)))))
      (check-equal?
        (merge-tc-results (list (ret null null null -Symbol 'x) (ret null null null -String 'x)))
        (ret null null null (Un -Symbol -String) 'x))
    )


    (test-suite "values->tc-results"
      (check-equal?
        (values->tc-results (make-Values (list (-result -Symbol))) (list -empty-obj) (list Univ))
        (ret -Symbol))

      (check-equal?
        (values->tc-results (make-Values (list (-result -Symbol) (-result -String))) 
                            (list -empty-obj -empty-obj) (list Univ Univ))
        (ret (list -Symbol -String)))

      (check-equal?
        (values->tc-results (make-Values (list (-result -Symbol (-PS -tt -ff)))) (list -empty-obj) (list Univ))
        (ret -Symbol (-PS -tt -ff)))

      (check-equal?
        (values->tc-results (make-Values (list (-result -Symbol (-PS -tt -ff) (make-Path null '(0 . 0)))))
                            (list -empty-obj) (list Univ))
        (ret -Symbol (-PS -tt -ff)))

      (check-equal?
        (values->tc-results (make-Values (list (-result (-opt -Symbol) (-PS (-is-type '(0 . 0) -String) -tt))))
                            (list -empty-obj) (list Univ))
        (ret (-opt -Symbol) -tt-propset))

      (check-equal?
        (values->tc-results (make-Values (list (-result (-opt -Symbol) (-PS (-not-type '(0 . 0) -String) -tt))))
                            (list -empty-obj) (list Univ))
        (ret (-opt -Symbol) -tt-propset))

      (check-equal?
        (values->tc-results (make-Values (list (-result (-opt -Symbol) (-PS (-not-type '(0 . 0) -String) -tt)
                                                 (make-Path null '(0 . 0)))))
                            (list (make-Path null #'x)) (list Univ))
        (ret (-opt -Symbol) (-PS (-not-type #'x -String) -tt) (make-Path null #'x)))

      ;; Check additional props
      (check-equal?
        (values->tc-results (make-Values (list (-result (-opt -String) (-PS -tt (-not-type '(0 . 0) -String))
                                                 (make-Path null '(0 . 0)))))
                            (list (make-Path null #'x)) (list -String))
        (ret -String -true-propset (make-Path null #'x)))

      ;; Substitute into ranges correctly
      (check-equal?
        (values->tc-results (make-Values (list (-result (-opt (-> Univ -Boolean : (-PS (-is-type '(0 . 0) -Symbol) -tt))))))
                            (list (make-Path null #'x)) (list Univ))
        (ret (-opt (-> Univ -Boolean : (-PS (-is-type '(0 . 0) -Symbol) -tt)))))

      (check-equal?
        (values->tc-results (make-Values (list (-result (-opt (-> Univ -Boolean : (-PS (-is-type '(1 . 0) -Symbol) -tt))))))
                            (list (make-Path null #'x)) (list Univ))
        (ret (-opt (-> Univ -Boolean : (-PS (-is-type #'x -Symbol) -tt)))))

      ;; Substitute into prop of any values
      (check-equal?
        (values->tc-results (make-AnyValues (-is-type '(0 . 0) -String))
                            (list (make-Path null #'x)) (list Univ))
        (-tc-any-results (-is-type #'x -String)))


      (check-equal?
        (values->tc-results (-values-dots null (-> Univ -Boolean : (-PS (-is-type '(1 . 0) -String) -tt)) 'b)
                            (list (make-Path null #'x)) (list Univ))
        (ret null null null (-> Univ -Boolean : (-PS (-is-type #'x -String) -tt)) 'b))

      ;; Prop is restricted by type of object
      (check-equal?
        (values->tc-results (make-Values (list (-result -Boolean (-PS (-is-type '(0 . 0) -PosReal) (-is-type '(0 . 0) -NonPosReal)))))
                            (list (make-Path null #'x)) (list -Integer))
        (ret -Boolean (-PS (-is-type #'x -PosInt) (-is-type #'x -NonPosInt))))

      ;; Prop restriction accounts for paths
      (check-equal?
        (values->tc-results
         (make-Values
          (list (-result -Boolean
                         (-PS (make-TypeProp (make-Path (list -car) '(0 . 0))
                                             -PosReal)
                              (make-TypeProp (make-Path (list -car) '(0 . 0))
                                             -NonPosReal)))))
         (list (make-Path null #'x))
         (list (-lst -Integer)))
        (ret -Boolean
             (-PS (make-TypeProp (make-Path (list -car) #'x) -PosInt)
                  (make-TypeProp (make-Path (list -car) #'x) -NonPosInt))))
    )

    (test-suite "term substitution"
      (check-equal?
        (abstract-obj (ret Univ -tt-propset (make-Path null #'x))
                      (list #'x))
        (ret Univ -tt-propset (make-Path null '(0 . 0))))
      (check-equal?
        (abstract-obj (ret (-> Univ Univ : -tt-propset : (make-Path null #'x)))
                      (list #'x))
        (ret (-> Univ Univ : -tt-propset : (make-Path null '(1 . 0)))))
      (check-equal?
       (abstract-obj (ret (-refine/fresh y -Int (-leq (-lexp y) (-lexp #'x)))
                          -tt-propset
                          (make-Path null #'x))
                     (list #'x))
        (ret (-refine/fresh y -Int (-leq (-lexp y) (-lexp (make-Path null '(1 . 0)))))
             -tt-propset
             (make-Path null '(0 . 0))))

      (check-equal?
       (erase-identifiers
        (ret (-refine/fresh y -Int (-leq (-lexp y) (-lexp #'x)))
             -tt-propset
             (make-Path null #'x))
        (list #'x))
       (ret -Int
            -tt-propset
            -empty-obj))
    )
  ))
