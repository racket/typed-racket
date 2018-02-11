#lang racket/base

;; Unit tests for typed units

(require (submod "typecheck-tests.rkt" test-helpers)
         "test-utils.rkt"
         (for-syntax racket/base
                     typed-racket/tc-setup
                     typed-racket/utils/tc-utils))

(provide tests)
(gen-test-main)

;; see typecheck-tests.rkt for rationale on imports
(require rackunit
         typed/racket/contract
         typed-racket/utils/utils
         (except-in (base-env extra-procs prims base-types base-types-extra)
                    define lambda λ case-lambda)
         (prefix-in tr: (only-in (base-env prims) define lambda λ case-lambda))
         (for-syntax (rep type-rep prop-rep object-rep values-rep)
                     (rename-in (types abbrev union numeric-tower prop-ops utils)
                                [Un t:Un]
                                [-> t:->])))
(define tests
  (test-suite
   "contract typechecking tests"
   (test-suite
    "basic combinators"
    [tc-e (flat-named-contract 'positive? (>/c 0))
          (-FlatContract Univ -Real)]
    [tc-e any/c (-FlatContract Univ Univ)]
    [tc-e none/c (-FlatContract Univ Univ)]
    [tc-e (not/c (lambda (x) (and (exact-integer? x)
                                  (>= x 10))))
          (-FlatContract Univ Univ)]
    [tc-e (=/c 13) (-FlatContract Univ -Nat)]
    [tc-e (</c 31) (-FlatContract Univ -Real)]
    [tc-e (>/c 10) (-FlatContract Univ -Real)]
    [tc-e (<=/c 24) (-FlatContract Univ -Real)]
    [tc-e (>=/c 43) (-FlatContract Univ -Real)]
    [tc-e (between/c 13 31) (-FlatContract Univ -Real)]
    [tc-e (real-in 24 43) (-FlatContract Univ -Real)]
    [tc-e (integer-in 17 71) (-FlatContract Univ -PosInt)]
    [tc-e (string-len/c 3) (-FlatContract Univ -String)]
    [tc-e natural-number/c (-FlatContract Univ -Nat)]
    [tc-e false/c (-Contract Univ -False)]
    [tc-e printable/c (-FlatContract Univ Univ)]
    [tc-e (listof (>/c 5)) (-Contract (-lst Univ) (-lst -Real))]
    [tc-e (listof string?) (-Contract (-lst Univ) (-lst -String))]
    [tc-e (non-empty-listof real?) (-Contract (-lst Univ) (-pair -Real (-lst -Real)))]
    [tc-e (list*of boolean?) (-Contract (-mu x (-pair Univ (t:Un Univ x)))
                                   (-mu x (-pair -Boolean (t:Un -Boolean x))))]
    [tc-e (cons/c even? odd?) (-Contract Univ (-pair -Integer -Integer))]
    [tc-e (list/c (>/c 5) (string-len/c 10) (</c 10) (string-len/c 5))
          (-Contract (-lst* Univ Univ Univ Univ)
                (-lst* -Real -String -Real -String))]
    [tc-e (list/c) (-Contract Univ (-lst*))]
    [tc-e (syntax/c even?) (-FlatContract (-Syntax -Integer) (-Syntax -Integer))]
    [tc-err (syntax/c (->/c any/c any/c))]
    [tc-e (parameter/c odd?) (-Contract (-Param -Integer) (-Param -Integer))]
    [tc-e (parameter/c string? odd?) (-Contract (-Param -String -Integer)
                                           (-Param -String -Integer))]
    ;; Might be nice to have the output type be (Contract Any (U 'a 'b 'c))
    [tc-e (symbols 'a 'b 'c) (-Contract Univ -Symbol)])

   (test-suite
    "->"
    [tc-e (->/c (string-len/c 5))
          (-Contract (t:-> Univ) (t:-> -String))]
    [tc-e (->/c exact-integer? any/c)
          (-Contract (t:-> -Integer Univ)
                (t:-> Univ Univ))]
    [tc-e (->/c (lambda (x) (exact-integer? x)) any/c)
          (-Contract (t:-> -Integer Univ)
                (t:-> Univ Univ))]
    [tc-e (let ()
            (define (int-gt-10 x)
              (and (exact-integer? x)
                   (> 10 x)))
            (define my-any/c any/c)
            (->/c my-any/c int-gt-10))
          (-Contract (t:-> Univ Univ)
                (t:-> Univ -Integer))]
    [tc-e (->/c (lambda (x) (exact-integer? x)) (</c 5) any/c)
          (-Contract (t:-> -Integer -Real Univ)
                (t:-> Univ Univ Univ))]
    [tc-e (->/c (->/c exact-integer? any/c) exact-integer?)
          (-Contract (t:-> (t:-> Univ Univ) Univ)
                (t:-> (t:-> -Integer Univ) -Integer))])

   (test-suite
    "->i"
    [tc-e (->i ([x real?]
                [y (x) (>=/c x)])
               [result (x y) (>=/c (+ x y))])
          (-Contract (t:-> -Real -Real Univ)
                (t:-> Univ Univ -Real))]
    ;; no dependencies, unnamed range
    [tc-e (->i ([x string?]
                [y exact-integer?]
                [z boolean?])
               [_ string?])
          (-Contract (t:-> -String -Integer -Boolean Univ)
                (t:-> Univ Univ Univ -String))]
    ;; mixed mandatory domains
    [tc-e (->i (#:x [x real?]
                [z exact-integer?]
                #:y [y (x) (>=/c x)])
               [_ string?])
          (-Contract (->key -Integer #:x -Real #t #:y -Real #t Univ)
                (->key Univ #:x Univ #t #:y Univ #t -String))]
    ;; mixed mandatory and optional doms
    [tc-e (->i ([x real?]
                [y real?])
               (#:w [w (x) (>=/c x)]
                [z (x y) (>=/c (+ x y))])
               [result (x y w z) (and/c exact-integer?
                                        (>=/c (+ x y w z)))])
          (-Contract (->optkey -Real -Real (-Real) #:w -Real #f Univ)
                (->optkey Univ Univ (Univ) #:w Univ #f -Integer))]
    ;; optional keyword doms + no mandatory doms; from ->i docs
    [tc-e (->i ()
               (#:x [x real?]
                #:y [y (x) (>=/c x)])
               [result (x y) (>=/c (+ x y))])
          (-Contract (->optkey () #:x -Real #f #:y -Real #f Univ)
                (->optkey () #:x Univ #f #:y Univ #f -Real))]
    [tc-e (->i ()
               [result real?])
          (-Contract (t:-> Univ)
                (t:-> -Real))]
    ;; #:rest, no dependency
    [tc-e (->i ()
               #:rest [xs (listof real?)]
               [result real?])
          (-Contract (->* (list) -Real Univ)
                (->* (list) Univ -Real))]
    [tc-err (let ()
              (->i ()
                   #:rest [xs number?]
                   [_ any/c])
              (void))
            #:ret (ret -Void -true-propset)
            #:msg "#:rest contract must be a list contract"]
    ;; dom depending on #:rest
    [tc-e (->i ([x (xs) (lambda: ([x : Integer]) (apply > x -inf.0 xs))])
               #:rest [xs (listof exact-integer?)]
               [_ any/c])
          (-Contract (->* (list -Integer) -Integer Univ)
                (->* (list -Integer) Univ Univ))]
    ;; rng depending on #:rest
    [tc-e (->i ()
               #:rest [xs (listof real?)]
               [sum (xs) (and/c real?
                                (=/c (apply + xs)))])
          (-Contract (->* (list) -Real Univ)
                (->* (list) Univ -Real))]
    ;; mult-value ranges
    [tc-e (->i ([x real?]
                [y real?])
               (values [r1 (x) (>/c x)]
                       [r2 (r1 y) (>/c (+ r1 y))]))
          (-Contract (t:-> -Real -Real (make-Values (list (-result Univ) (-result Univ))))
                (t:-> Univ Univ (make-Values (list (-result -Real) (-result -Real)))))]
    ;; various pre/post conditions
    [tc-e (->i () #:pre () #t [_ any/c])
          (-Contract (t:-> Univ) (t:-> Univ))]
    [tc-e (->i () [_ any/c] #:post () #t)
          (-Contract (t:-> Univ) (t:-> Univ))]
    [tc-e (->i () #:pre/name () "named-pre" #t [_ any/c])
          (-Contract (t:-> Univ) (t:-> Univ))]
    [tc-e (->i () [_ any/c] #:post/name () "named-post" #t)
          (-Contract (t:-> Univ) (t:-> Univ))]
    [tc-err (let ()
              (->i () #:pre/desc () 0 [_ any/c])
              (void))
            #:ret (ret -Void -true-propset)
            #:msg #rx"expected:(.*(Boolean|String|\\(Listof String\\)))+"]
    [tc-err (let ()
              (->i () [_ any/c] #:post/desc () 0)
              (void))
            #:ret (ret -Void -true-propset)
            #:msg #rx"expected:(.*(Boolean|String|\\(Listof String\\)))+"]
    [tc-e (->i ([x natural-number/c]
                [y natural-number/c])
               [result (x y) (=/c (+ x y))]
               #:post (result x y)
               (and (= (- result x) y)
                    (= (- result y) x)))
          (-Contract (t:-> -Nat -Nat Univ)
                (t:-> Univ Univ -Nat))])

   (test-suite
    "coercible literals"
    ;; we use or/c to coerce the literals to a contract type
    [tc-e (or/c (list))
          (-Contract Univ -Null)]
    [tc-e (or/c 'a 'b 'c 'λ)
          (-Contract Univ (t:Un (-val 'a) (-val 'b) (-val 'c) (-val 'λ)))]
    [tc-e (or/c #f #t)
          (-Contract Univ -Boolean)]
    [tc-e (or/c '#:key '#:words)
          (-Contract Univ (t:Un (-val '#:key) (-val '#:words)))]
    [tc-e (or/c #\a #\b #\c #\λ)
          (-Contract Univ -Char)]
    [tc-e (or/c "foo" "bar")
          (-Contract Univ -String)]
    (tc-e (or/c '#"baz" '#"quux")
          (-Contract Univ -Bytes))
    [tc-e (or/c 5 6 7)
          (-Contract Univ -PosByte)]
    [tc-e (or/c #rx"[01]+" #rx"[0123456789A-Fa-f]+")
          (-Contract Univ -String)]
    [tc-e (or/c #rx#"01234" #rx#"abcd")
          (-Contract Univ -Bytes)])

   (test-suite
    "and/c, or/c"
    [tc-e (and/c exact-integer? (</c 5))
          (-Contract Univ -Integer)]
    [tc-e (and/c real? exact-integer?)
          (-Contract Univ -Integer)]
    [tc-e (and/c)
          (-Contract Univ Univ)]
    [tc-e (and/c (and/c exact-integer? real?) exact-integer?)
          (-Contract Univ -Integer)]
    [tc-e (and/c string? exact-integer?)
          (-Contract Univ (t:Un))]
    [tc-e/t (let ()
              (: app-any/c (All (a) (-> (FlatContract Any Any) a Boolean)))
              (define (app-any/c any/c x)
                (any/c x))
              app-any/c)
            (-poly (a) (t:-> (-FlatContract Univ Univ) a -Boolean))]
    [tc-e (or/c exact-integer?)
          (-Contract Univ -Integer)]
    [tc-e (or/c (>/c 5) (string-len/c 10))
          (-Contract Univ (t:Un -Real -String))]
    [tc-e (or/c (>/c 5) exact-integer?)
          (-Contract Univ (t:Un -Real -Integer))]
    [tc-e (or/c false/c exact-integer?)
          (-Contract Univ (t:Un -False -Integer))]
    [tc-e (and/c (lambda: ([x : Real]) (> x 10)))
          (-Contract -Real -PosReal)]
    [tc-e (and/c exact-integer? (lambda: ([x : Integer]) (> x 0)))
          (-Contract Univ -PosInt)]
    [tc-e (and/c exact-integer? string?)
          (-Contract Univ (t:Un))]
    [tc-err (let ()
              (and/c exact-integer? (lambda: ([x : Positive-Integer]) (even? x))))
            #:ret (ret (-Contract Univ -PosInt) -true-propset)
            #:msg #rx"type mismatch"]
    [tc-err (and/c exact-integer? (lambda: ([x : String]) (equal? x "foo")))
            #:ret (ret (-Contract Univ (t:Un)) -true-propset)])

   (test-suite
    "misc"
    [tc-e (and/c (->/c exact-integer? exact-integer?)
                 (->/c zero? zero?))
          (-Contract (t:-> -Integer Univ)
                (t:-> -Number -Zero))]
    [tc-e (contract even? 5 'pos 'neg)
          -PosByte]
    [tc-err (contract even? "five" 'pos 'neg)]
    [tc-e (contract (->/c even? even?)
                    (ann add1 (-> Integer Integer))
                    'pos 'neg)
          (t:-> -Integer -Integer)])))
