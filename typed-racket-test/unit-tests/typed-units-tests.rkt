#lang racket/base

;; Unit tests for typed units

(require (submod "typecheck-tests.rkt" test-helpers)
         (except-in "test-utils.rkt" private)
         (for-syntax racket/base
                     typed-racket/tc-setup
                     typed-racket/utils/tc-utils))

(provide tests)
(gen-test-main)

(begin-for-syntax
  ;; for checking the printing of type aliases
  (current-type-names (init-current-type-names)))

;; see typecheck-tests.rkt for rationale on imports
(require rackunit
         typed/racket/unit
         (except-in typed-racket/utils/utils private)
         (except-in (base-env extra-procs prims class-prims
                              base-types base-types-extra)
                    define lambda λ case-lambda)
         (prefix-in tr: (only-in (base-env prims) define lambda λ case-lambda))
         (for-syntax (rep type-rep prop-rep object-rep)
                     (rename-in (types abbrev numeric-tower prop-ops utils)
                                [Un t:Un]
                                [-> t:->])))

(define tests
  (test-suite
   "unit typechecking tests"
   ;; simple tests that annotations work appropriately
   [tc-e
    (let ()
      (define-signature a^ ([a : Integer]))
      (unit
        (import)
        (export a^)
        (: a Natural)
        (define a 5))
      (void))
    -Void]
   [tc-err
    (let ()
      (define-signature a^ ([a : Integer]))
      (unit
        (import)
        (export a^)
        (: a String)
        (define a 5))
      (error ""))]
   [tc-e
    (let ()
      (define-signature a^ ([a : Integer]))
      (unit
        (import)
        (export a^)
        (: a Integer)
        (define a 5)
        (: b String)
        (define b "foo"))
      (void))
    -Void]
   [tc-err
    (let ()
      (define-signature a^ ([a : Integer]))
      (unit
        (import)
        (export a^)
        (: a Integer)
        (define a 5)
        (: b String)
        (define b 5)e)
      (error ""))]

   [tc-e
    (let ()
      (define-signature a^ ([a : Integer]))
      (define-type-alias Foo (Unit (import a^) (export) Integer))
      (define a 17)
      (: u (Unit (import a^) (export) Integer))
      (define u (unit (import a^) (export) a))
      (invoke-unit u (import a^)))
    -Integer]

   [tc-e
    (let ()
      (define-type-alias Foo (U Integer (Unit (import a^) (export) Foo)))
      (define-signature a^ ([a : Foo]))
      (: u Foo)
      (define u (unit (import a^) (export) a))
      (define a 11)
      (invoke-unit (assert u unit?) (import a^))
      (void))
    -Void]

   #|
   This should typecheck, but doesn't because fact has no type annotation
   
   (define-signature fact^
     ([fact : (-> Integer Integer)]))



   (define-unit fact@
   (import (prefix i: fact^))
   (export fact^)
   (define (fact n) 
   (if (< n 1) 
   1
   (* n (i:fact (sub1 n)))))
   fact)
   |#
  [tc-e
   (let ()
     (define-signature a^ ())
     (define-signature b^ ())
     
     (define-unit u
       (import a^)
       (export))

     (define-unit v
       (import)
       (export a^ b^)
       5)

     (define-compound-unit/infer w
       (import)
       (export)
       (link (() u A)
             (([A : a^] [B : b^]) v)))
     
     (define-compound-unit/infer w2
       (import [A : a^])
       (export)
       (link (() u A)))

     (define-compound-unit/infer w3
       (import) 
       (export)
       (link u v))
     
     (define-compound-unit/infer w4
       (import a^)
       (export)
       (link u))
     (void))
   -Void]
   
   [tc-err
    (let ()
      (unit (import)
            (export)
        (+ 1 "bad"))
      (error ""))]

   ;; factorial with units
   [tc-e
    (let ()
      (define-signature fact^ ([fact : (-> Integer Natural)]))
      
      (define-unit fact@
        (import (prefix i: fact^))
        (export fact^)
        (: fact (-> Integer Natural))
        (define (fact n)
          (if (<= n 1) 1 (* n (i:fact (sub1 n)))))
        fact)
      
      (define-compound-unit/infer fact-c@
        (import)
        (export)
        (link fact@))
      
      (define factorial (invoke-unit fact-c@))
      
      (factorial 5))
    -Nat]

   ;; factorial + invoke-unit/infer
   [tc-e
    (let ()
      (define-signature fact^ ([fact : (-> Integer Natural)]))
      (define-unit fact@
        (import (prefix i: fact^))
        (export fact^)
        (: fact (-> Integer Natural))
        (define (fact n)
          (if (<= n 1) 1 (* n (i:fact (sub1 n)))))
        fact)
      (define fact (invoke-unit/infer fact@))
      (fact 5))
    -Nat]
   [tc-e
    (let ()
      (define a 11)
      (let ()
        (define-signature a^ ([a : Integer]))
        (define-unit u
          (import a^)
          (export)
          (: x Integer)
          (define x a)
          x)
        (invoke-unit/infer u)))
    -Integer]

   ;; Make sure scoping doesn't break the typed unit expansion/typechecking
   [tc-e
    (let ()
      (define-signature x^ ([x : Number]))
      (define u 
        (unit 
          (import x^)
          (export)
          (unit (import x^) (export) x)))
      (void))
    -Void]
   [tc-e
    (let ()
      (define-signature x^ ([x : Number]))
      (unit 
        (import)
        (export x^)
        (define x^ "FOOBAR")
        (define x 5))
      (void))
    -Void]

   [tc-e
    (let ()
      (define-signature x^ ([x : (-> Integer Integer)]))
      (unit 
        (import x^)
        (export)
        (define-signature x^ ([y : (-> Natural Real)]))
        (unit
          (import)
          (export x^)
          (define y x))
        (void))
      (void))
    -Void]

   ;; Tests adapted from racket-tests/tests/units/test-unit.rkt
   [tc-e
    (begin (invoke-unit (unit (import) (export) 12)) (void))
    -Void]

   [tc-e
    (let ()
      (define-signature x-sig ([x : Integer]))
      (define-signature y-sig ([y : Integer]))
      (define-signature z-sig ([z : Integer]))
      (define-signature empty-sig ())

      (invoke-unit
       (compound-unit (import) (export)
                      (link (((X : x-sig) (Y : y-sig)) (unit (import empty-sig z-sig)
                                                         (export y-sig x-sig)
                                                         (define x 1)
                                                         (define y 2))
                             Z E)
                            (((Z : z-sig) (E : empty-sig)) (unit (import x-sig y-sig)
                                                             (export empty-sig z-sig)
                                                             (define z 3)
                                                             3) X Y)))))
    -PosByte]

   [tc-e
    (let ()
      (define-signature x-sig ([x : Integer]))
      (define-signature xy-sig ([x : Integer] [y : Integer]))
      (define-signature b-sig ([b : Integer]))

      (let ((un (compound-unit (import) (export S U)
                               (link (((S : x-sig)) (unit (import) (export x-sig) (define x 10)))
                                     (((U : xy-sig)) (unit (import) (export xy-sig) (define x 11) (define y 12)))))))
        (invoke-unit
         (compound-unit (import) (export)
                        (link (((S : x-sig) (U : xy-sig)) un)
                              (((B : b-sig)) (unit (import x-sig) (export b-sig) (define b x)) S)
                              (() (unit (import b-sig xy-sig) (export) (list b x y)) B U)))))
      (void))
    -Void]

   [tc-e
    (let ()
      (define-signature even-sig ([even : (-> Integer Boolean)]))
      (define-signature odd-sig ([odd : (-> Integer Boolean)]))
      
      (define even-unit
        (unit (import odd-sig)
          (export even-sig)
          (: even (-> Integer Boolean))
          (define (even x)
            (or (= 0 x) (odd (sub1 x))))))
      
      (define odd-unit
        (unit (import even-sig)
        (export odd-sig)
        (: odd (-> Integer Boolean))
        (define (odd x)
          (and (> x 0) (even (sub1 x))))
        (define x (odd 11))
        x))
      
      (define run-unit
        (compound-unit (import)
                       (export)
                       (link (((EVEN : even-sig)) even-unit ODD)
                             (((ODD : odd-sig)) odd-unit EVEN))))

      (invoke-unit run-unit))
    -Boolean]

   [tc-e
    (let ()
      (define-signature x-sig ([x : Integer]))
      (define-signature yz-sig ([y : Integer] [z : Integer]))
      (invoke-unit
        (compound-unit
         (import)
         (export)
         (link (((T : yz-sig))
                (unit (import x-sig) (export yz-sig)
                  (define-values (y a) (values 1 2))
                  (define-values (b z) (values y a)))
                S)
               (((S : x-sig))
                (unit (import yz-sig) (export x-sig) (define x 3) (+ y z)) T)))))
    -Integer]

   [tc-e
    (let ()
      (define-signature x-sig ([x : Integer]))
      (invoke-unit
       (unit 
         (import)
         (export x-sig)
         (define-syntax (y stx)
           (syntax-case stx ()
             ((_ x) #'(define x 1))))
         (y x)
         x)))
    -One]

   [tc-e
    (let ()
      (define-signature fact-sig ([fact : (-> Natural Natural)] [n : Natural]))
      (invoke-unit
       (compound-unit (import) (export)
                      (link (((F : fact-sig)) (unit (import (except (rename fact-sig (f-in fact)) n))
                                                (export (rename fact-sig (f-out fact)))
                                                (define n 1)
                                                (: f-out (-> Natural Natural))
                                                (define (f-out x) (if (= 0 x)
                                                                      1
                                                                      (* x (f-in (sub1 x))))))
                             F)
                            (() (unit (import (only fact-sig fact)) (export)
                                  (define n 2)
                                  (fact 4))
                             F)))))
    -Nat]

   [tc-e
    (let ()
      (define-signature x-sig ([x : Integer]))
      (invoke-unit
       (compound-unit (import) (export)
                      (link (((S : x-sig)) (unit (import) (export x-sig) (define x 1)))
                            (() (unit (import (prefix s: x-sig)) (export) s:x) S)))))
    -Integer]

   [tc-e
    (let ()
      (define-signature x-sig ([x : Integer]))
      (define-signature xy-sig ([x : Integer] [y : Integer]))
      (define-signature yz-sig ([y : Integer] [z : Integer]))
      (define-signature sx ([x : Integer]))
      (define-signature sy ([y : Integer]))
      (define-signature sz ([z : Integer]))
      (invoke-unit
       (compound-unit (import) (export)
                      (link (((S : x-sig) (T : yz-sig) (U : xy-sig))
                             (unit (import) (export (rename x-sig (s:x x))
                                                    (rename yz-sig (t:y y) (t:z z))
                                                    (prefix u: xy-sig))
                               (define x 1) (define y 2) (define z 3)
                               (define s:x x) (define t:y y) (define t:z z) (define u:x x) (define u:y y)))
                            (((SX : sx)) (unit (import (prefix s: x-sig)) (export sx) (define x s:x)) S)
                            (((SY : sy)) (unit (import (prefix u: xy-sig)) (export sy) (define y u:y)) U)
                            (((SZ : sz)) (unit (import (prefix t: yz-sig)) (export sz) (define z t:z)) T)
                            (() (unit (import sx sy sz) (export) (+ x y z)) SX SY SZ)))))
    -Integer]

   [tc-e
    (let ()
      (define-signature b-sig ([b : Integer]))
      (let ([b : String "WRONG b"])
        (define u1 (unit (import) (export b-sig) (define b 2)))
        (define u2 (unit (import b-sig) (export) b))
        (invoke-unit (compound-unit (import) (export)
                                    (link (((B : b-sig)) u1)
                                          (() u2 B))))))
    -Integer]

   ;; subtyping tests
   [tc-e
    (let ()
      (define-signature x-sig ([x : Integer]))
      (define-signature y-sig ([y : Integer]))
      (define-signature x-sub extends x-sig ([xx : Integer]))
      (define-signature y-sub extends y-sig ([yy : Integer]))
            
      (define u1 (unit (import x-sig) (export y-sub) (define y (add1 x)) (define yy 2) (+ x y yy)))
      (define u2 (unit (import y-sig) (export x-sub) (define x 3) (define xx 44)))

      (invoke-unit
         (compound-unit (import) (export)
                        (link (((S1 : x-sig)) u2 S2)
                              (((S2 : y-sig)) u1 S1)))))
    -Integer]
   ;; Make sure let expressions order the signature processing correctly
   [tc-e
    (let ()
      (let ()
        (define-signature y-sig ([y : Integer]))
        (define-signature y-sub extends y-sig ([yy : String]))
        (unit (import) (export y-sub) (define y 1) (define yy "yy is a string"))
        (void)))
    -Void]

   ;; check that invoke-unit and invoke-unit/infer both work correctly
   [tc-e
    (let ()
      (define-signature a^ ([a : Integer]))
      (define-signature aa^ extends a^ ([aa : String]))

      (define-unit u (import a^) (export) a)
      (define a 1)
      (define aa "One")
      (invoke-unit/infer u))
    -Integer]

   [tc-e
    (let ()
      (define-signature a^ ([a : Integer]))
      (define-signature aa^ extends a^ ([aa : String]))

      (define-unit u (import a^) (export) a)
      (define a 1)
      (define aa "One")
      (invoke-unit u (import aa^)))
    -Integer]

   ;; Sanity checking combinations of compound-unit and invoke-unit
   [tc-e
    (let ()
      (define-signature a^ ([a : Integer]))
      (define u@ (unit (import) (export a^) (define a 5)))
      (invoke-unit
       (compound-unit
        (import)
        (export)
        (link (((a^1 : a^)) u@)
              (()
               (unit
                 (import a^)
                 (export)
                 (values a))
               a^1))) (import)))
    -Integer]

   [tc-e
    (let ()
      (define-signature a^ ([a : Integer]))
      (define-unit u@ (import) (export a^) (define a 5))
      (invoke-unit
       (compound-unit
        (import)
        (export)
        (link (((a^1 : a^)) u@)
              (()
               (unit
                 (import a^)
                 (export)
                 (values a))
               a^1))) (import)))
    -Integer]

    ;; make sure that invoke-unit/infer works when there is a link clause
    [tc-e
     (let ()
       (define-signature a^ ([a : Integer]))

       (define-unit u (import) (export a^) (define a 5))
       (define-unit v (import a^) (export) a)

       (invoke-unit/infer (link u v)))
     -Integer]

    ;; unit-from-context and define-unit-from-context tests
    [tc-e
     (let ()
       (define-signature a^ ([a : Integer]))
       (define a 17)
       (define-unit u (import a^) (export) a)
       (define-unit-from-context v a^)
       (define w (compound-unit/infer (import) (export) (link v u)))
       (invoke-unit w))
     -Integer]

    [tc-e
     (let ()
       (define-signature a^ ([a : Integer]))
       (define a 17)
       (define-unit u (import a^) (export) a)
       (define-unit-from-context v a^)
       (invoke-unit/infer (link v u)))
     -Integer]

    [tc-e
     (let ()
       (define-signature a^ ([a : Integer]))
       (define a 17)
       (define u (unit-from-context a^))
       (define v (unit (import a^) (export) a))
       (invoke-unit
        (compound-unit
         (import)
         (export)
         (link (([A : a^]) u)
               (() v A)))
        (import)))
     -Integer]

    ;; Make sure body annotations for exports are checked
    [tc-err
     (let ()
       (define-signature a^ ([a : Integer]))
       (unit
         (import)
         (export a^)
         (: a String)
         (define a 5))
       (error ""))]
    ;; and for non-exports as well
    [tc-err
     (let ()
       (define-signature a^ ([a : Integer]))
       (unit
         (import)
         (export a^)
         (: b String)
         (define b 12)
         (define a 5))
       (error ""))]

   ;; init-depends type errors with compound-unit/define-compound-unit
   [tc-err
    (let ()
      (define-signature a ())
      (define-signature aa extends a ())
      (define-unit u1
        (import )
        (export aa))
      (define-unit u2
        (import a)
        (export)
        (init-depend a))
      (define-unit u3
        (import)
        (export aa))
      (compound-unit
       (import [A1 : a])
       (export)
       (link
        (([A2 : a]) u1 )
        (() u2 A)
        (([A : aa]) u3)))
      (error ""))]

   [tc-err
    (let ()
      (define-signature a ())
      (define-unit u1
        (import )
        (export a))
      (define-unit u2
        (import a)
        (export)
        (init-depend a))
      (define-unit u3
        (import)
        (export a))
      (compound-unit
       (import [A1 : a])
       (export)
       (link
        (([A2 : a]) u1 )
        (() u2 A)
        (([A : a]) u3)))
      (error ""))]

   [tc-err
    (let ()
      (define-signature a ())
      (define-signature aa extends a ())
      (define-unit u1
        (import )
        (export aa))
      (define-unit u2
        (import aa)
        (export))
      (define-unit u3
        (import)
        (export))
      (compound-unit
       (import)
       (export)
       (link
        (([A : a]) u1 )
        (() u2 A)))
      (error ""))]

   [tc-err
    (let ()
      (define-signature a ())
      (define-unit u1
        (import )
        (export a))
      (define-unit u2
        (import a)
        (export)
        (init-depend a))
      (define-unit u3
        (import)
        (export a))
      (define-compound-unit w
        (import [A1 : a])
        (export)
        (link
         (([A2 : a]) u1 )
         (() u2 A)
         (([A : a]) u3)))
      (error ""))]

   ;; inference
   [tc-err
    (let ()
      (invoke-unit/infer 1)
      (error ""))]

   [tc-err 
    (let ()
      (compound-unit (import) (export) (link (() 1)))
      (error ""))]

   [tc-err
    (let ()
      (define-signature x-sig ([x : Integer]))
      (compound-unit (import) (export)
                     (link (() (unit (import x-sig) (export)))))
      (error ""))]

   [tc-err
    (let ()
      (define-signature x-sig ([x : Integer]))
      (compound-unit (import) (export)
                     (link (([X : x-sig]) (unit (import) (export)))))
      (error ""))]

   [tc-err
    (let ()
      (invoke-unit 1)
      (error ""))]

   [tc-err
    (let ()
      (define-signature x-sig ([x : Integer]))
      (invoke-unit (unit (import x-sig) (export) x))
      (error ""))]

   [tc-err
    (let ()
      (define-signature x^ ([x : String]))
      (unit (import x^) (export)
        (: y Integer)
        (define y x)
        y)
      (error ""))]
   
   ;; Type mismatch in unit body
   [tc-err
    (let ()  
      (unit (import) (export)
        (: x String)
        (define x 5))
      (error ""))]
   [tc-err
    (let ()
      (define-signature a^ ([a : String]))
      (unit (import) (export a^) (define a 5))
      (error ""))]
   [tc-err
    (let ()
      (define-signature a^ ([a : Integer]))
      (define a "foo")
      (invoke-unit (unit (import a^) (export) a) (import a^))
      (error ""))]
   [tc-err
    (let ()
      (define-signature a^ ([a : Integer]))
      (unit
        (import a^)
        (export)
        (: x String)
        (define x a))
      (error ""))]
   ;; units can only import/export distinct sets of signatures
   [tc-err
    (let ()
      (define-signature a^ ())
      (define-signature b^ extends a^ ())
      (define-siganture c^ extends a^ ())
      (unit (import b^ c^) (export))
      (error ""))]

   ;; invoking a unit which imports a locally defined signature is a type error
   ;; even if it is invoked with an import of the same name
   [tc-err
    (let ()
      (define-signature bad^ ())
      (define bad
        (let ()
          (define-signature bad^ ())
          (unit (import bad^) (export))))
      (invoke-unit bad (import bad^))
      (error ""))]
   
   ;; This tests that the linking clauses in compound-unit forms
   ;; are correctly satisfied
   ;; This is fairly subtle, and the type mismatch error
   ;; message doesn't make it obvious why it fails to typecheck
   [tc-err
    (let ()
      (define-signature a^ ())
       
      (define-unit a1
        (import)
        (export a^))
       
      (define-unit a2
        (import)
        (export a^))
       
      (define-unit u
        (import a^)
        (export)
        (init-depend a^))
       
      (define-compound-unit w
        (import)
        (export)
        (link
         (([A1 : a^]) a1)
         (() u A2)
         (([A2 : a^]) a2)))
      (error ""))]))

