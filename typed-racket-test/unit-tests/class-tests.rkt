#lang racket/base

;; Unit tests for typed classes

(require (submod "typecheck-tests.rkt" test-helpers)
         (except-in "test-utils.rkt" private)
         (for-syntax racket/base
                     (submod "typecheck-tests.rkt" test-helpers)
                     typed-racket/tc-setup
                     typed-racket/utils/tc-utils))

(provide tests)
(gen-test-main)

(begin-for-syntax
  ;; for checking the printing of type aliases
  (current-type-names (init-current-type-names)))

;; see typecheck-tests.rkt for rationale on imports
(require rackunit
         typed/racket/class
         (except-in typed-racket/utils/utils private)
         (except-in (base-env extra-procs prims class-prims
                              base-types base-types-extra)
                    define lambda λ case-lambda)
         (prefix-in tr: (only-in (base-env prims) define lambda λ case-lambda))
         (for-syntax (rep type-rep prop-rep object-rep)
                     (rename-in (types abbrev union numeric-tower prop-ops utils)
                                [Un t:Un]
                                [-> t:->])))

(define tests
  (test-suite
   "class typechecking tests"
   #reader typed-racket/typed-reader
   ;; Basic class with init and public method
   [tc-e (let ()
           (: c% (Class (init [x Integer])
                        [m (Integer -> Integer)]))
           (define c%
             (class object%
               (super-new)
               (init x)
               (define/public (m x) 0)))
           (send (new c% [x 1]) m 5))
         -Integer]
   ;; Fails, bad superclass expression
   [tc-err (let ()
             (: d% (Class (init [x Integer])
                          [m (Integer -> Integer)]))
             (define d% (class 5
                          (super-new)
                          (init x)
                          (define/public (m x) 0)))
             (void))
           #:ret (tc-ret -Void)
           #:msg #rx"superclass expression should produce a class"]
   ;; Method using argument type
   [tc-e (let ()
           (: e% (Class (init [x Integer])
                        [m (Integer -> Integer)]))
           (define e% (class object%
                        (super-new)
                        (init x)
                        (define/public (m x) x)))
           (void))
         -Void]
   ;; Send inside a method
   [tc-e (let ()
           (: f% (Class (init [x Integer])
                        [m (Integer -> Integer)]))
           (define f% (class object%
                        (super-new)
                        (init x)
                        (define/public (m x) (send this m 3))))
           (void))
         -Void]
   ;; Fails, send to missing method
   [tc-err (let ()
             (: g% (Class (init [x Integer #:optional])
                          [m (Integer -> Integer)]))
             (define g% (class object%
                          (super-new)
                          (init [x 0])
                          (define/public (m x) (send this z))))
             (void))
           #:ret (tc-ret -Void)
           #:msg #rx"method not understood.*method name: z"]
   ;; Send to other methods
   [tc-e (let ()
           (: h% (Class [n (-> Integer)]
                        [m (Integer -> Integer)]))
           (define h% (class object%
                        (super-new)
                        (define/public (n) 0)
                        (define/public (m x) (send this n))))
           (void))
         -Void]
   ;; Local sends
   [tc-e (let ()
           (: i% (Class [n (-> Integer)]
                        [m (Integer -> Integer)]))
           (define i% (class object%
                        (super-new)
                        (define/public (n) 0)
                        (define/public (m x) (n))))
           (void))
         -Void]
   ;; Send to non object
   [tc-err (send 4 m 3)
      #:ret (tc-ret -Bottom -ff-propset)
      #:expected (tc-ret -Bottom -ff-propset)]
   ;; Fails, sending to multiple/unknown values
   [tc-err (send (values 'a 'b) m 'c)
           #:msg #rx"expected single value"]
   [tc-err (send (eval "3") m 'c)
           #:msg #rx"expected single value"]
   ;; Send to a union of objects in various ways
   [tc-e (let ()
           (: f (-> (U (Object [m (-> String)])
                       (Object [m (-> Symbol)]
                               [n (-> Void)]))
                    (U Symbol String)))
           (define (f o) (send o m))
           (f (new (class object% (super-new) (define/public (m) "foo")))))
         (t:Un -String -Symbol)]
   [tc-e (let ()
           (: f (-> (U (Object [m (-> (values String Symbol))])
                       (Object [m (-> (values Symbol String))]
                               [n (-> Void)]))
                    (values (U Symbol String) (U Symbol String))))
           (define (f o) (send o m))
           (f (new (class object% (super-new)
                     (define/public (m) (values "foo" 'bar))))))
         #:ret (tc-ret (list (t:Un -String -Symbol) (t:Un -String -Symbol)))]
   [tc-err
    (let ()
      (define obj
        (if (> (random) 0.5)
            (new (class object% (super-new)
                   (define/public (m) "foo")))
            (new (class object% (super-new)
                   (define/public (m) (values "foo" "bar"))))))
      (send obj m))
    #:msg #rx"Expected the same number of values.*got 1 and 2"]
   ;; Field access via get-field
   [tc-e (let ()
           (: j% (Class (field [n Integer])
                        [m (-> Integer)]))
           (define j% (class object%
                        (super-new)
                        (field [n 0])
                        (define/public (m) (get-field n this))))
           (void))
         -Void]
   ;; Test set-field!
   [tc-e (set-field! x
           (new (class object%
                  (super-new)
                  (field [x : String "foo"])))
           "bar")
         -Void]
   ;; fails, check set-field! type error
   [tc-err (set-field! x
             (new (class object%
                    (super-new)
                    (field [x : String "foo"])))
             'not-string)
           #:ret (tc-ret -Void)
           #:msg #rx"set-field! only allowed with"]
   ;; fails, field's default value has wrong type
   [tc-err (class object% (super-new)
             (: x Symbol)
             (field [x "foo"]))
           #:ret (tc-ret (-class #:field ([x -Symbol])))
           #:msg #rx"expected: Symbol.*given: String"]
   ;; Fail, field access to missing field
   [tc-err (let ()
             (: k% (Class [m (-> Integer)]))
             (define k% (class object%
                          (super-new)
                          (define/public (m) (get-field n this))))
             (void))
           #:ret (tc-ret -Void)
           #:msg #rx"missing an expected field.*field: n"]
   ;; Fail, conflict with parent field
   [tc-err (let ()
             (: j% (Class (field [n Integer])
                          [m (-> Integer)]))
             (define j% (class object%
                          (super-new)
                          (field [n 0])
                          (define/public (m) (get-field n this))))
             (: l% (Class (field [n Integer])
                          [m (-> Integer)]))
             (define l% (class j%
                          (field [n 17])
                          (super-new)))
             (void))
           #:ret (tc-ret -Void)
           #:msg #rx"has a conflicting public field.*field: n"]
   ;; Fail, conflict with parent method
   [tc-err (let ()
             (: j% (Class [m (-> Integer)]))
             (define j% (class object%
                          (super-new)
                          (define/public (m) 15)))
             (: m% (Class [m (-> Integer)]))
             (define m% (class j%
                          (super-new)
                          (define/public (m) 17)))
             (void))
           #:ret (tc-ret -Void)
           #:msg #rx"has a conflicting public method.*method: m"]
   ;; Inheritance
   [tc-e (let ()
           (: j% (Class (field [n Integer])
                        [m (-> Integer)]))
           (define j% (class object%
                        (super-new)
                        (field [n 0])
                        (define/public (m) (get-field n this))))
           (: n% (Class (field [n Integer])
                        [m (-> Integer)]))
           (define n% (class j% (super-new)))
           (void))
         -Void]
   ;; fail, superclass expression is not a class with no expected type
   [tc-err (class "foo" (super-new))
           #:ret (tc-ret (-class))
           #:msg "expected: a class"]
   ;; should fail, too many methods
   [tc-err (let ()
             (: o% (Class))
             (define o% (class object%
                          (super-new)
                          (define/public (m) 0)))
             (void))
           #:ret (tc-ret -Void)
           #:msg #rx"method `m' that is not in expected type"]
   ;; same as previous
   [tc-err (let ()
             (: c% (Class [m (Integer -> Integer)]))
             (define c% (class object% (super-new)
                          (define/public (m x) (add1 x))
                          (define/public (n) 0)))
             (void))
           #:ret (tc-ret -Void)
           #:msg #rx"method `n' that is not in expected type"]
   ;; fails, too many inits
   [tc-err (let ()
             (: c% (Class))
             (define c% (class object% (super-new)
                          (init x)))
             (void))
           #:ret (tc-ret -Void)
           #:msg #rx"init `x' that is not in expected type"]
   ;; fails, init should be optional but is mandatory
   [tc-err (let ()
             (: c% (Class (init [str String #:optional])))
             (define c% (class object% (super-new)
                          (init str)))
             (void))
           #:ret (tc-ret -Void)
           #:msg #rx"expected: optional init `str'.*given: mandatory init `str'"]
   ;; fails, too many fields
   [tc-err (let ()
             (: c% (Class (field [str String])))
             (define c% (class object% (super-new)
                          (field [str "foo"] [x 0])))
             (void))
           #:ret (tc-ret -Void)
           #:msg #rx"field `x' that is not in expected type"]
   ;; test that an init with no annotation still type-checks
   ;; (though it will have the Any type)
   [tc-e (let () (class object% (super-new) (init x)) (void)) -Void]
   ;; test that a field with no annotation still type-checks
   ;; (though it will have the Any type)
   [tc-e (let () (class object% (super-new) (field [x 0])) (void)) -Void]
   ;; Mixin on classes without row polymorphism
   [tc-e (let ()
           (: mixin ((Class [m (-> Integer)])
                     ->
                     (Class [m (-> Integer)]
                            [n (-> String)])))
           (define (mixin cls)
             (class cls
               (super-new)
               (define/public (n) "hi")))

           (: arg-class% (Class [m (-> Integer)]))
           (define arg-class%
             (class object%
               (super-new)
               (define/public (m) 0)))

           (mixin arg-class%)
           (void))
         -Void]
   ;; Fail, bad mixin
   [tc-err (let ()
             (: mixin ((Class [m (-> Integer)])
                       ->
                       (Class [m (-> Integer)]
                              [n (-> String)])))
             (define (mixin cls)
               (class cls
                 (super-new)))

             (: arg-class% (Class [m (-> Integer)]))
             (define arg-class%
               (class object%
                 (super-new)
                 (define/public (m) 0)))

             (mixin arg-class%))
           #:ret (tc-ret (-class #:method ([m (t:-> -Integer)] [n (t:-> -String)])))
           #:msg #rx"lacks expected method `n'"]
   ;; Fail, bad mixin argument
   [tc-err (let ()
             (: mixin ((Class [m (-> Symbol)])
                       ->
                       (Class [m (-> Symbol)]
                              [n (-> String)])))
             (define (mixin cls)
               (class cls
                 (super-new)
                 (define/public (n) "hi")))

             (: arg-class% (Class [k (-> Symbol)]))
             (define arg-class%
               (class object%
                 (super-new)
                 (define/public (k) 'k)))

             (mixin arg-class%)
             (void))
           #:ret (tc-ret -Void)
           #:msg #rx"lacks expected method `m'"]
   ;; classes that don't use define/public directly
   [tc-e (let ()
           (: c% (Class [m (Number -> String)]))
           (define c%
             (class object%
               (super-new)
               (public m)
               (define-values (m)
                 (lambda (x) (number->string x)))))
            (send (new c%) m 4))
         -String]
   ;; check that classes work in let clauses
   [tc-e (let: ([c% : (Class [m (Number -> String)])
                 (class object%
                   (super-new)
                   (public m)
                   (define-values (m)
                     (lambda (x) (number->string x))))])
           (send (new c%) m 4))
         -String]
   ;; check a good super-new call
   [tc-e (let ()
           (: c% (Class (init [x Integer])))
           (define c% (class object% (super-new) (init x)))
           (: d% (Class))
           (define d% (class c% (super-new [x (+ 3 5)])))
           (void))
         -Void]
   ;; fails, missing super-new
   [tc-err (let ()
             (: c% (Class (init [x Integer])))
             (define c% (class object% (init x)))
             (void))
           #:ret (tc-ret -Void)
           #:msg #rx"must call `super-new'"]
   ;; fails, non-top-level super-new
   ;; FIXME: this case also spits out additional untyped identifier
   ;;        errors which should be squelched maybe
   [tc-err (let ()
             (: c% (Class (init [x Integer])))
             (define c% (class object% (let () (super-new)) (init x)))
             (void))
           #:ret (tc-ret -Void)
           #:msg #rx"must call `super-new'"]
   ;; fails, bad super-new argument
   [tc-err (let ()
             (: c% (Class (init [x Symbol])))
             (define c% (class object% (super-new) (init x)))
             (: d% (Class))
             (define d% (class c% (super-new [x "bad"])))
             (void))
           #:ret (tc-ret -Void)
           #:msg #rx"expected: Symbol.*given: String"]
   ;; test override
   [tc-e (let ()
           (: c% (Class [m (Integer -> Integer)]))
           (define c% (class object% (super-new)
                        (define/public (m y) (add1 y))))
           (: d% (Class [m (Integer -> Integer)]))
           (define d% (class c% (super-new)
                        (define/override (m y) (* 2 y))))
           (void))
         -Void]
   ;; test local call to overriden method
   [tc-e (let ()
           (: c% (Class [m (Integer -> Integer)]))
           (define c% (class object% (super-new)
                        (define/public (m y) (add1 y))))
           (: d% (Class [n (Integer -> Integer)]
                        [m (Integer -> Integer)]))
           (define d% (class c% (super-new)
                        (define/public (n x) (m x))
                        (define/override (m y) (* 2 y))))
           (void))
         -Void]
   ;; fails, superclass missing public for override
   [tc-err (let ()
             (: d% (Class [m (String -> String)]))
             (define d% (class object% (super-new)
                          (define/override (m y)
                            (string-append (assert y string?) "foo"))))
             (void))
           #:ret (tc-ret -Void)]
   ;; local field access and set!
   [tc-e (let ()
           (: c% (Class (field [x Integer])
                        [m (Integer -> Integer)]))
           (define c% (class object% (super-new)
                        (field [x 0])
                        (define/public (m y)
                          (begin0 x (set! x (+ x 1))))))
           (void))
         -Void]
   ;; test top-level expressions in the class
   [tc-e (let ()
           (: c% (Class [m (Integer -> Integer)]))
           (define c% (class object% (super-new)
                        (define/public (m y) 0)
                        (+ 3 5)))
           (void))
         -Void]
   ;; test top-level method call
   [tc-e (let ()
           (: c% (Class [m (Integer -> Integer)]))
           (define c% (class object% (super-new)
                        (define/public (m y) 0)
                        (m 3)))
           (void))
         -Void]
   ;; test top-level field access
   [tc-e (let ()
           (: c% (Class (field [f String])))
           (define c% (class object% (super-new)
                        (field [f "foo"])
                        (string-append f "z")))
           (void))
         -Void]
   ;; fails, bad top-level expression
   [tc-err (let ()
             (: c% (Class [m (Symbol -> Symbol)]))
             (define c% (class object% (super-new)
                          (define/public (m y) 'a)
                          (string-append (string->symbol "a") "a")))
             (void))
           #:ret (tc-ret -Void)
           #:msg #rx"expected: String.*given: Symbol"]
   ;; fails, ill-typed method call
   [tc-err (let ()
             (: c% (Class [m (Symbol -> Symbol)]))
             (define c% (class object% (super-new)
                          (define/public (m y) 'a)
                          (m "foo")))
             (void))
           #:ret (tc-ret -Void)
           #:msg #rx"expected: Symbol.*given: String"]
   ;; fails, ill-typed field access
   [tc-err (let ()
             (: c% (Class (field [f String])))
             (define c% (class object% (super-new)
                          (field [f "foo"])
                          (set! f 'a)))
             (void))
           #:ret (tc-ret -Void)
           #:msg #rx"expected: String.*given: 'a"]
   ;; test private field
   [tc-e (let ()
           (class object%
             (super-new)
             (: x Integer)
             (define x 5)
             (set! x 8)
             (+ x 1))
           (: d% (Class (field [y String])))
           (define d%
             (class object%
               (super-new)
               (: x Integer)
               (define x 5)
               (: y String)
               (field [y "foo"])))
           (void))
         -Void]
   ;; fails, bad private field set!
   [tc-err (class object%
             (super-new)
             (: x Symbol)
             (define x 'a)
             (set! x "foo"))
           #:ret (tc-ret (-class))
           #:msg #rx"expected: Symbol.*given: String"]
   ;; fails, bad private field default
   [tc-err (class object%
             (super-new)
             (: x Symbol)
             (define x "foo"))
           #:ret (tc-ret (-class))
           #:msg #rx"expected: Symbol.*given: String"]
   ;; ok, synthesis works on private fields
   [tc-e (class object% (super-new)
           (define x "foo") (string-append x "bar"))
         (-class)]
   ;; private field with function
   [tc-e (class object%
           (super-new)
           (: f (-> String))
           (define (f) "foo"))
         (-class)]
   [tc-err (let ()
             (class object%
               (super-new)
               (: f (-> String))
               (define (f) 'bad))
             (error "foo"))
           #:msg #rx"type mismatch.*expected: String"]
   ;; multiple names in define-values private fields
   [tc-e (class object%
           (super-new)
           (define-values (x y z) (values 'x 'y 'z)))
         (-class)]
   ;; test private method
   [tc-e (let ()
           (class object% (super-new)
             (: x (-> Integer))
             (define/private (x) 3)
             (: m (-> Integer))
             (define/public (m) (x)))
           (void))
         -Void]
   ;; fails, public and private types conflict
   [tc-err (class object% (super-new)
             (: x (-> Symbol))
             (define/private (x) 'a)
             (: m (-> String))
             (define/public (m) (x)))
           #:ret (tc-ret (-class #:method ([m (t:-> -String)])))
           #:msg #rx"expected: String.*given: Symbol"]
   ;; fails, not enough annotation on private
   [tc-err (class object% (super-new)
             (define/private (x) 3)
             (: m (-> Integer))
             (define/public (m) (x)))
           #:ret (tc-ret (-class #:method ([m (t:-> -Integer)])))
           #:msg #rx"Cannot apply expression of type Any"]
   ;; fails, ill-typed private method implementation
   [tc-err (class object% (super-new)
             (: x (-> Symbol))
             (define/private (x) "bad result"))
           #:ret (tc-ret (-class))
           #:msg #rx"expected: Symbol.*given: String"]
   ;; test optional init arg
   [tc-e (let ()
           (: c% (Class (init [x Integer #:optional])))
           (define c% (class object% (super-new)
                        (: x Integer)
                        (init [x 0])))
           (void))
         -Void]
   ;; test init coverage when all optionals are
   ;; in the superclass
   [tc-e (let ()
           (: c% (Class (init [x Integer #:optional])))
           (: d% (Class (init [x Integer #:optional])))
           (define c% (class object% (super-new)
                        (: x Integer)
                        (init [x 0])))
           (define d% (class c% (super-new)))
           (void))
         -Void]
   ;; fails, expected mandatory but got optional
   [tc-err (let ()
             (: c% (Class (init [x Integer])))
             (define c% (class object% (super-new)
                          (: x Integer)
                          (init [x 0])))
             (void))
           #:ret (tc-ret -Void)
           #:msg #rx"expected: mandatory init `x'.*given: optional init `x'"]
   ;; fails, mandatory init not provided
   [tc-err (let ()
             (define d% (class object% (super-new)
                          (: x Integer)
                          (init x)))
             (new d%))
           #:ret (tc-ret (-object #:init ([x -Integer #f])))
           #:msg #rx"value not provided for named init arg x"]
   ;; test that provided super-class inits don't count
   ;; towards the type of current class
   [tc-e (let ()
           (: c% (Class))
           (define c% (class (class object% (super-new)
                               (: x Integer)
                               (init x))
                        (super-new [x 3])))
           (void))
         -Void]
   ;; fails, super-class init already provided
   [tc-err (let ()
             (define c% (class (class object% (super-new)
                                 (: x Integer)
                                 (init x))
                          (super-new [x 3])))
             (new c% [x 5]))
           #:ret (tc-ret (-object))]
   ;; fails, super-new can only be called once per class
   [tc-err (class object% (super-new) (super-new))
           #:ret (tc-ret (-class))
           #:msg #rx"`super-new' a single time"]
   ;; test passing an init arg to super-new
   [tc-e (let ()
           (define c% (class (class object% (super-new)
                               (: x Integer)
                               (init x))
                        (: x Integer)
                        (init x)
                        (super-new [x x])))
           (new c% [x 5])
           (void))
         -Void]
   ;; fails, bad argument type to super-new
   [tc-err (class (class object% (super-new)
                    (: x Integer)
                    (init x))
             (: x String)
             (init x)
             (super-new [x x]))
           #:ret (tc-ret (-class #:init ([x -String #f])))]
   ;; fails, superclass does not accept this init arg
   [tc-err (class object% (super-new [x 3]))
           #:ret (tc-ret (-class))
           #:msg "not accepted by superclass"]
   ;; test inherit method
   [tc-e (let ()
           (class (class object% (super-new)
                    (: m (Integer -> Integer))
                    (define/public (m x) (add1 x)))
             (super-new)
             (inherit m)
             (m 5))
           (void))
         -Void]
   ;; test inherit method in another method (next 3)
   [tc-e (let ()
           (class (class object% (super-new)
                    (: m (-> String String))
                    (define/public (m x) (string-append x "m")))
             (super-new)
             (inherit m)
             (: n (-> String))
             (define/public (n) (m "foo")))
           (void))
         -Void]
   [tc-e (let ()
           (class (class object% (super-new)
                    (: m (-> String String))
                    (define/public (m x) (string-append x "m")))
             (super-new)
             (inherit m)
             (define/public (n) (m "foo")))
           (void))
         -Void]
   [tc-e (let ()
           (class (class object% (super-new)
                    (: m (-> String String))
                    (define/public (m x) (string-append x "m")))
             (super-new)
             (inherit m)
             (define/private (n) (m "foo")))
           (void))
         -Void]
   ;; test internal name with inherit
   [tc-e (let ()
           (class (class object% (super-new)
                    (: m (Integer -> Integer))
                    (define/public (m x) (add1 x)))
             (super-new)
             (inherit [n m])
             (n 5))
           (void))
         -Void]
   ;; test inherit field
   [tc-e (let ()
           (class (class object% (super-new)
                    (field [x : Integer 0]))
             (super-new)
             (inherit-field x))
           (void))
         -Void]
   ;; test internal name with inherit-field
   [tc-e (let ()
           (class (class object% (super-new)
                    (field [x : String "b"]))
             (super-new)
             (inherit-field [y x])
             (set! y "a"))
           (void))
         -Void]
   ;; fails, superclass missing inherited field
   [tc-err (class (class object% (super-new))
             (super-new)
             (inherit-field [y x]))
           #:ret (tc-ret (-class))
           #:msg #rx"superclass is missing a required field"]
   ;; fails, missing super method for inherit
   [tc-err (class (class object% (super-new)) (super-new) (inherit z))
           #:ret (tc-ret (-class))]
   ;; fails, bad argument type to inherited method
   [tc-err (class (class object% (super-new)
                    (: m (Integer -> Integer))
                    (define/public (m x) (add1 x)))
             (super-new)
             (inherit m)
             (m "foo"))
           #:ret (tc-ret (-class #:method ([m (t:-> -Integer -Integer)])))]
   ;; test that keyword methods type-check
   [tc-e (let ()
           (: c% (Class [n (Integer #:foo Integer -> Integer)]))
           (define c%
             (class object%
               (super-new)
               (define/public (n x #:foo foo)
                 (+ foo x))))
           (send (new c%) n 0 #:foo 1))
         -Integer]
   ;; fails, bad kw argument
   [tc-err (let ()
             (: c% (Class [n (Integer #:foo Integer -> Integer)]))
             (define c%
               (class object%
                 (super-new)
                 (define/public (n x #:foo foo)
                   (+ foo x))))
             (send (new c%) n 0 #:foo "foo")
             (error "foo"))
           #:msg #rx"expected Integer.*got String"]
   ;; test instance subtyping
   [tc-e (let ()
           (define c%
             (class object%
               (super-new)
               (: x (U False Number))
               (field [x 0])))
           (: x (Instance (Class)))
           (define x (new c%))
           (void))
         -Void]
   ;; failing instance subtyping
   [tc-err (let ()
             (define x (new (class object% (super-new) (define/public (m) "m"))))
             (ann x (Object [n (-> String)]))
             (error "foo"))
           #:msg #rx"lacks expected method `n'"]
   [tc-err (let ()
             (define x (new (class object% (super-new))))
             (ann x (Object (field [x String])))
             (error "foo"))
           #:msg #rx"lacks expected field `x'"]
   [tc-err (let ()
             (define x (new (class object% (super-new) (define/public (m) "m"))))
             (ann x (Object [m (-> Symbol)]))
             (error "foo"))
           #:msg #rx"expected: \\(-> Symbol\\).*given: \\(-> String"]
   [tc-err (let ()
             (define x (new (class object% (super-new) (field [x : Symbol 'x]))))
             (ann x (Object (field [x String])))
             (error "foo"))
           #:msg #rx"expected: String.*given: Symbol"]
   ;; test use of `this` in field default
   [tc-e (let ()
           (class object%
             (super-new)
             (: x Integer)
             (field [x 0])
             (: y Integer)
             (field [y (get-field x this)]))
           (void))
         -Void]
   ;; test super calls
   [tc-e (let ()
           (define c%
             (class object%
               (super-new)
               (: m (String -> String))
               (define/public (m x) "a")))
           (define d%
             (class c%
               (super-new)
               (define/override (m x)
                 (string-append "x" (super m "b")))))
           (send (new d%) m "c"))
         -String]
   ;; test super calls at top-level
   [tc-e (let ()
           (define c%
             (class object%
               (super-new)
               (: m (Integer -> Integer))
               (define/public (m x) 0)))
           (define d%
             (class c%
               (super-new)
               (super m 5)
               (define/override (m x) 5)))
           (void))
         -Void]
   ;; fails, bad super call argument
   [tc-err (let ()
             (define c%
               (class object%
                 (super-new)
                 (: m (Integer -> Integer))
                 (define/public (m x) 0)))
             (define d%
               (class c%
                 (super-new)
                 (super m "foo")
                 (define/override (m x) 5))))]

   ;; test different internal/external names
   [tc-e (let ()
           (define c% (class object% (super-new)
                        (public [m n])
                        (define m (lambda () 0))))
           (send (new c%) n)
           (void))
         -Void]
   ;; test local calls with internal/external
   [tc-e (let ()
           (define c% (class object% (super-new)
                        (: m (-> String))
                        (public [m n])
                        (define m (lambda () "a"))
                        (: z (-> String))
                        (define/public (z) (m))))
           (send (new c%) z))
         -String]
   ;; internal/external the same is ok
   [tc-e (let ()
           (define c% (class object% (super-new)
                        (public [m m])
                        (define m (lambda () "a"))))
           (send (new c%) m))
         #:ret (tc-ret -String -true-propset)]
   ;; fails, internal name not accessible
   [tc-err (let ()
             (define c% (class object% (super-new)
                          (public [m n])
                          (define m (lambda () 0))))
             (send (new c%) m))]
   ;; test internal/external with expected
   [tc-e (let ()
           (: c% (Class [n (-> String)]))
           (define c% (class object% (super-new)
                        (public [m n])
                        (define m (lambda () "a"))))
           (send (new c%) n))
         -String]
   ;; test internal/external field
   [tc-e (let ()
           (define c% (class object% (super-new)
                        (: f String)
                        (field ([f g] "a"))))
           (get-field g (new c%)))
         -String]
   ;; fail, internal name not accessible
   [tc-err (let ()
             (define c% (class object% (super-new)
                          (: f String)
                          (field ([f g] "a"))))
             (get-field f (new c%)))]
   ;; test internal/external init
   [tc-e (let ()
           (define c% (class object% (super-new)
                        (: i String)
                        (init ([i j]))))
           (new c% [j "a"])
           (void))
         -Void]
   ;; fails, internal name not accessible
   [tc-err (let ()
             (define c% (class object% (super-new)
                          (: i Integer)
                          (init ([i j]))))
             (new c% [i 5]))
           #:ret (tc-ret (-object #:init ([j -Integer #f])))]
   ;; test that different internal names can map to the same external name
   ;; and that the internal-external name mapping is set correctly.
   [tc-e (class object%
           (super-new)
           (: x* String)
           (init [(x* x)])
           (field [x "x"]))
         (-class #:init ([x -String #f]) #:field ([x Univ]))]
   ;; test init default values
   [tc-e (let ()
           (class object% (super-new)
             (: z Integer)
             (init [z 0]))
           (void))
         -Void]
   ;; fails, bad default init value
   [tc-err (class object% (super-new)
             (: z Integer)
             (init [z "foo"]))
           #:ret (tc-ret (-class #:init ([z -Integer #t])))
           #:msg #rx"expected: Integer.*given: String"]
   ;; test init field default value
   [tc-e (let ()
           (define c% (class object% (super-new)
                 (: x Integer)
                 (init-field ([x y] 0))))
           (void))
         -Void]
   ;; fails, wrong init-field default
   [tc-err (class object% (super-new)
             (: x Integer)
             (init-field ([x y] "foo")))
           #:ret (tc-ret (-class #:init ([y -Integer #t]) #:field ([y -Integer])))]
   ;; test type-checking method with internal/external
   [tc-err (let ()
             (: c% (Class [n (Integer -> Integer)]))
             (define c% (class object% (super-new)
                          (public [m n])
                          (define m (lambda () 0)))))]
   ;; test type-checking without expected class type
   [tc-e (let ()
           (define c% (class object% (super-new)
                        (: m (String -> String))
                        (define/public (m x) "a")))
           (send (new c%) m "b"))
         -String]
   ;; fails, because the local call type is unknown
   ;; and is assumed to be Any
   [tc-err (class object% (super-new)
             (define/public (m) (n))
             (define/public (n x) 0))
           #:ret (tc-ret (-class #:method ([m (t:-> -Bottom)] [n (t:-> Univ -Zero : -true-propset)])))
           #:msg #rx"since it is not a function type"]
   ;; test type-checking for classes without any
   ;; internal type annotations on methods
   [tc-e (let ()
           (define c% (class object% (super-new)
                        (define/public (m) "a")))
           (send (new c%) m))
         #:ret (tc-ret -String -true-propset)]
   ;; test inheritance without expected
   [tc-e (let ()
           (define c% (class (class object% (super-new)
                               (: m (-> String))
                               (define/public (m) "a"))
                        (super-new)
                        (: n (-> String))
                        (define/public (n) "b")))
           (send (new c%) m)
           (send (new c%) n))
         -String]
   ;; test fields without expected class type
   [tc-e (let ()
           (define c% (class object% (super-new)
                        (: x String)
                        (field [x "a"])))
           (get-field x (new c%)))
         -String]
   ;; row polymorphism, basic example with instantiation
   [tc-e (let ()
           (: f (All (A #:row (field x))
                  ((Class #:row-var A)
                   ->
                   (Class #:row-var A (field [x Integer])))))
           (define (f cls)
             (class cls (super-new)
               (field [x 5])))
           (row-inst f (Row (field [y Integer])))
           (void))
         -Void]
   ;; fails, because the instantiation uses a field that
   ;; is supposed to be absent via the row constraint
   [tc-err (let ()
             (: f (All (A #:row (field x))
                    ((Class #:row-var A)
                     ->
                     (Class #:row-var A (field [x Integer])))))
             (define (f cls)
               (class cls (super-new)
                 (field [x 5])))
             (row-inst f (Row (field [x Integer]))))
           #:ret (tc-ret (t:-> (-class 
                              #:row (make-Row null `([x ,-Integer]) null null #f))
                            (-class
                              #:row (make-Row null `([x ,-Integer]) null null #f)
                              #:field ([x -Integer])))
                      -true-propset)]
   ;; fails, mixin argument is missing required field
   [tc-err (let ()
             (: f (All (A #:row (field x))
                    ((Class #:row-var A)
                     ->
                     (Class #:row-var A (field [x Integer])))))
             (define (f cls)
               (class cls (super-new)
                 (field [x 5])))
             (define instantiated
               (row-inst f (Row (field [y Integer]))))
             (instantiated
              (class object% (super-new))))
           #:ret (tc-ret (-class
                        #:row (make-Row null (list (list 'y -Integer)) null null #f)
                        #:field ([x -Integer])))]
   ;; fails, the argument object lacks required fields (with inference)
   [tc-err (let ()
             (: mixin (All (r #:row)
                         (-> (Class (field [x Any]) #:row-var r)
                             (Class (field [x Any]) #:row-var r))))
             (define (mixin cls) cls)
             (mixin object%))
           #:ret (tc-ret (-class #:row (make-Row null null null null #f)
                              #:field ([x Univ])))
           #:msg #rx"lacks expected field `x'"]
   ;; mixin application succeeds
   [tc-e (let ()
           (: f (All (A #:row (field x))
                  ((Class #:row-var A)
                   ->
                   (Class #:row-var A (field [x Integer])))))
           (define (f cls)
             (class cls (super-new)
               (field [x 5])))
           (define instantiated
             (row-inst f (Row (field [y Integer]))))
           (instantiated
            (class object% (super-new)
              (: y Integer)
              (field [y 0])))
           (void))
         -Void]
   ;; test row instantiation with other clauses
   [tc-e (let ()
           (: f (All (A #:row (field x))
                  ((Class #:row-var A)
                   ->
                   (Class #:row-var A (field [x Integer])))))
           (define (f cls)
             (class cls (super-new)
               (field [x 5])))
           (define instantiated
             (row-inst f (Row [m (-> Void)]
                              (augment [m (-> Void)])
                              (init [y Integer #:optional])
                              (field [y Integer])
                              (init-rest (List String)))))
           (instantiated
            (class object% (super-new)
              (: y Integer)
              (init-field [y 0])
              (init-rest [rst : (List String)])
              (define/pubment (m) (void))))
           (void))
         -Void]
   ;; Basic row constraint inference
   [tc-e (let ()
           (: f (All (A #:row) ; inferred
                  ((Class #:row-var A)
                   ->
                   (Class #:row-var A (field [x Integer])))))
           (define (f cls)
             (class cls (super-new)
               (field [x 5])))
           (row-inst f (Row (field [y Integer])))
           (void))
         -Void]
   ;; fails, inferred constraint and instantiation don't match
   [tc-err (let ()
             (: f (All (A #:row)
                    ((Class #:row-var A)
                     ->
                     (Class #:row-var A (field [x Integer])))))
             (define (f cls)
               (class cls (super-new)
                 (field [x 5])))
             (row-inst f (Row (field [x Integer]))))
           #:ret (tc-ret (t:-> (-class 
                              #:row (make-Row null `([x ,-Integer]) null null #f))
                            (-class
                              #:row (make-Row null `([x ,-Integer]) null null #f)
                              #:field ([x -Integer])))
                      -true-propset)]
   ;; test bad manipulation of rows for inheritance
   [tc-e (let ()
           (: c% (Class (init [x String] [y String])))
           (define c% (class object% (super-new) (init x y)))
           (: f (All (r #:row) (-> (Class #:row-var r) (Class #:row-var r))))
           (define (f x) x)
           ;; should have the same type as c%
           (define c2% (f c%))
           (: d% (Class (init [y String])))
           ;; should be the same as inheriting from c%
           (define d% (class c2% (super-new [x "foo"])))
           (void))
         -Void]
   ;; Check simple use of pubment
   [tc-e (let ()
           (define c%
             (class object%
               (super-new)
               (: m : String -> String)
               (define/pubment (m x) "a")))
           (send (new c%) m "b"))
         -String]
   ;; Local calls to pubment method
   [tc-e (let ()
           (define c%
             (class object%
               (super-new)
               (: m : String -> String)
               (define/pubment (m x) "a")
               (: n (-> String))
               (define/public (n) (m "b"))))
           (send (new c%) n))
         -String]
   ;; Inheritance with augment
   [tc-e (let ()
           (define c%
             (class object%
               (super-new)
               (: m : String -> String)
               (define/pubment (m x) "a")))
           (define d%
             (class c%
               (super-new)
               (define/augment (m x) (string-append x "b"))))
           (send (new c%) m "c"))
         -String]
   ;; Pubment with inner
   [tc-e (let ()
           (define c%
             (class object%
               (super-new)
               (: m : String -> String)
               (define/pubment (m x)
                 (inner "a" m x))))
           (define d%
             (class c%
               (super-new)
               (define/augment (m x)
                 (string-append "foo" x))))
           (send (new c%) m "b"))
         -String]
   ;; make sure augment type is reflected in class type
   [tc-e (let ()
           (: c% (Class (augment [m (String -> Integer)])
                        [m (Integer -> Integer)]))
           (define c%
             (class object% (super-new)
               (: m (Integer -> Integer)
                  #:augment (String -> Integer))
               (define/pubment (m x) x)))
           (void))
         -Void]
   ;; pubment with different augment type
   [tc-e (let ()
           (define c%
             (class object%
               (super-new)
               (: m (Symbol -> Symbol)
                  #:augment (String -> String))
               (define/pubment (m x)
                 (inner "" m "foo") 'a)))
           (define d%
             (class c%
               (super-new)
               (define/augment (m x)
                 (string-append x "bar"))))
           (send (new c%) m 'b))
         -Symbol]
   ;; fail, bad inner argument
   [tc-err (let ()
             (define c%
               (class object%
                 (super-new)
                 (: m (Symbol -> Symbol)
                    #:augment (String -> String))
                 (define/pubment (m x)
                   (inner "" m x) 'a)))
             (define d%
               (class c%
                 (super-new)
                 (define/augment (m x)
                   (string-append x "bar"))))
             (send (new c%) m 'b))
           #:ret (tc-ret -Symbol)
           #:msg #rx"expected: String.*given: Symbol"]
   ;; Fail, bad inner default
   [tc-err (class object%
             (super-new)
             (: m (Symbol -> Symbol))
             (define/pubment (m x)
               (inner "foo" m x)))
           #:ret (tc-ret (-class #:method ([m (t:-> -Symbol -Symbol)])
                              #:augment ([m (t:-> -Symbol -Symbol)])))
           #:msg #rx"expected: Symbol.*given: String"]
   ;; Fail, wrong number of arguments to inner
   [tc-err (class object%
             (super-new)
             (: m (Integer -> Integer))
             (define/pubment (m x)
               (inner 3 m)))
           #:ret (tc-ret (-class #:method ([m (t:-> -Integer -Integer)])
                              #:augment ([m (t:-> -Integer -Integer)])))
           #:msg #rx"wrong number of arguments provided.*expected: 2"]
   ;; Fail, bad augment type
   [tc-err (let ()
             (define c%
               (class object%
                 (super-new)
                 (: m (Symbol -> Symbol))
                 (define/pubment (m x)
                   (inner 'a m x))))
             (define d%
               (class c%
                 (super-new)
                 (define/augment (m x) "bad type")))
             (void))
           #:ret (tc-ret -Void)
           #:msg #rx"expected: Symbol.*given: String"]
   ;; Fail, cannot augment non-augmentable method
   [tc-err (let ()
             (define c%
               (class object%
                 (super-new)
                 (: m (Integer -> Integer))
                 (define/public (m x) 0)))
             (define d%
               (class c%
                 (super-new)
                 (define/augment (m x) 1)))
             (void))
           #:ret (tc-ret -Void)
           #:msg #rx"superclass is missing a required augmentable method"]
   ;; Pubment with separate internal/external names
   [tc-e (let ()
           (define c%
             (class object%
               (super-new)
               (: n (Symbol -> Symbol))
               (pubment [n m])
               (define n (λ (x) 'a))))
           (send (new c%) m 'b))
         -Symbol]
   ;; Pubment with expected class type
   [tc-e (let ()
           (: c% (Class [m (String -> String)]
                        (augment [m (String -> String)])))
           (define c%
             (class object%
               (super-new)
               (define/pubment (m x) "a")))
           (send (new c%) m "b"))
         -String]
   ;; fails, expected type not a class
   [tc-err (let ()
             (: c% String)
             (define c%
               (class object%
                 (super-new)
                 (: x Symbol)
                 (init-field x)))
             (void))
           #:ret (tc-ret -Void)
           #:msg #rx"expected: String"]
   ;; test polymorphic class
   [tc-e (let ()
           (: c% (All (A) (Class (init-field [x A]))))
           (define c%
             (class object%
               (super-new)
               (init-field x)))
           (new (inst c% Integer) [x 0])
           (void))
         -Void]
   ;; fails due to ill-typed polymorphic class body
   [tc-err (let ()
             (: c% (All (A) (Class (init-field [x A]))))
             (define c%
               (class object%
                 (super-new)
                 (init-field x)
                 (set! x "a")))
             (void))
           #:ret (tc-ret -Void)
           #:msg #rx"expected: A.*given: String"]
   ;; test polymorphism with keyword
   [tc-e (let ()
           (define point%
             (class object%
               #:∀ (X)
               (super-new)
               (init-field [x : X] [y : X])))
           (new (inst point% Integer) [x 0] [y 5])
           (new (inst point% String) [x "foo"] [y "bar"])
           (void))
         -Void]
   ;; test polymorphism with two type parameters
   [tc-e (let ()
           (define point%
             (class object%
               #:forall (X Y)
               (super-new)
               (init-field [x : X] [y : Y])))
           (new (inst point% Integer String) [x 0] [y "foo"])
           (new (inst point% String Integer) [x "foo"] [y 3])
           (void))
         -Void]
   ;; test class polymorphism with method
   [tc-e (let ()
           (define id%
             (class object%
               #:forall (X)
               (super-new)
               (: m (X -> X))
               (define/public (m x) x)))
           (send (new (inst id% String)) m "a"))
         -String]
   ;; fails because m is not parametric
   [tc-err (class object%
             #:forall (X)
             (super-new)
             (: m (X -> X))
             (define/public (m x) "a"))
           #:ret (tc-ret (-poly (X) (-class #:method ([m (t:-> X X)]))))
           #:msg #rx"expected: X.*given: String"]
   ;; fails because default init value cannot be polymorphic
   [tc-err (class object%
             #:forall (Z)
             (super-new)
             (init-field [x : Z] [y : Z 0]))
           #:ret (tc-ret (-poly (Z) (-class #:init-field ([x Z #f] [y Z #t]))))
           #:msg #rx"expected: Z.*given: Zero"]
   ;; fails because default field value cannot be polymorphic
   [tc-err (class object%
             #:forall (Z)
             (super-new)
             (field [x : Z "a"]))
           #:ret (tc-ret (-poly (Z) (-class #:field ([x Z]))))
           #:msg #rx"expected: Z.*given: String"]
   ;; test in-clause type annotations (next several tests)
   [tc-e (let ()
           (define c%
             (class object%
               (super-new)
               (field [x : String "a"])))
           (string-append "b" (get-field x (new c%))))
         -String]
   [tc-e (let ()
           (define c%
             (class object%
               (super-new)
               (init-field [x : String "a"])))
           (string-append "c" (get-field x (new c% [x "b"]))))
         -String]
   [tc-e (let ()
           (define c%
             (class object%
               (super-new)
               (public [m : (String -> String)])
               (define (m x) (string-append x "foo"))))
           (send (new c%) m "bar"))
         -String]
   [tc-e (let ()
           (define c%
             (class object%
               (super-new)
               (private [m : (String -> String)])
               (define (m x) (string-append x "foo"))))
           (void))
         -Void]
   [tc-e (let ()
           (define c%
             (class object%
               (super-new)
               (field [(x y) : String "a"])))
           (string-append "foo" (get-field y (new c%))))
         -String]
   ;; fails, duplicate type annotation
   [tc-err (class object%
             (super-new)
             (: x String)
             (field [x : Symbol 0]))
           #:ret (tc-ret (-class #:field ([x -String])))
           #:msg #rx"duplicate type annotation.*new type: Symbol"]
   ;; fails, expected type and annotation don't match
   [tc-err (let ()
             (: c% (Class (field [x String])))
             (define c% (class object% (super-new)
                          (field [x : Symbol 'a])))
             (void))
           #:ret (tc-ret -Void)
           #:msg #rx"expected: String.*given: Symbol"]
   ;; fails, but make sure it's not an internal error
   [tc-err (class object% (super-new)
             (define/pubment (foo x) 0)
             (define/public (g x) (foo 3)))
           #:ret (tc-ret (-class #:method ([g (t:-> Univ -Bottom)]
                                        [foo (t:-> Univ -Zero : -true-propset)])
                              #:augment ([foo top-func])))
           #:msg #rx"Cannot apply expression of type Any"]
   ;; the next several tests are for positional init arguments
   [tc-e (let ()
           (define c% (class object% (super-new) (init a b)))
           (new c% [a "a"] [b "b"])
           (make-object c% "a" "b")
           (instantiate c% ("a") [b "b"])
           (void))
         -Void]
   [tc-e (let ()
           (define c% (class object% (super-new) (init a [b 'b])))
           (new c% [a "a"] [b "b"])
           (new c% [a "a"])
           (make-object c% "a")
           (make-object c% "a" "b")
           (instantiate c% () [a "a"] [b "b"])
           (instantiate c% ("a") [b "b"])
           (instantiate c% ("a" "b"))
           (void))
         -Void]
   [tc-e (let ()
           (define c% (class (class object%
                               (super-new)
                               (init [b 'b]))
                        (super-new) (init [a 'a])))
           (new c% [a "a"] [b "b"])
           (new c% [b "b"])
           (new c% [a "a"])
           (make-object c% "a")
           (make-object c% "a" "b")
           (instantiate c% () [a "a"] [b "b"])
           (instantiate c% ("a") [b "b"])
           (instantiate c% ("a" "b"))
           (void))
         -Void]
   [tc-e (let ()
           (define c% (class object%
                        (super-new)
                        (init-rest [rst : (List String String)])))
           (make-object c% "a" "b")
           (void))
         -Void]
   [tc-e (let ()
           (define c% (class object%
                        (super-new)
                        (init [a : Symbol])
                        (init-rest [rst : (List String String)])))
           (make-object c% 'a "b" "c")
           (void))
         -Void]
   [tc-e (let ()
           (define c% (class object%
                        (super-new)
                        (init-rest [rst : (U (List Symbol)
                                             (List String String))])))
           (make-object c% "b" "c")
           (make-object c% 'a)
           (void))
         -Void]
   [tc-err (let ()
             (define c% (class object%
                          (super-new)
                          (init-rest [rst : (List Symbol)])))
             (make-object c% "wrong"))
           #:ret (tc-ret (make-Instance (make-Class #f null null null null (-Tuple (list -Symbol)))))
           #:msg #rx"expected: \\(List Symbol.*given: \\(List String"]
   ;; PR 14408, test init-field order
   [tc-e (let ()
           (define c%
             (class object%
               (super-new)
               (init-field [x : String] [y : Symbol])))
           (make-object c% "str" 'sym)
           (void))
         -Void]
   ;; a variant of the last, but testing that init and init-field
   ;; interleave correctly in the class type
   [tc-e (let ()
           (define c%
             (class object%
               (super-new)
               (init [a : 'a]) (init-field [x : 'x] [y : 'y]) (init [b 'b])))
           (make-object c% 'a 'x 'y 'b)
           (void))
         -Void]
   ;; fail, too many positional arguments to superclass
   [tc-err (class object% (super-make-object "foo"))
           #:ret (tc-ret (-class))
           #:msg #rx"too many positional init arguments"]
   ;; check that case-lambda methods work
   [tc-e (let ()
           (class object%
             (super-new)
             (field [y : Integer 0])
             (: m (case-> (Any -> Integer)))
             (public m)
             (define m (case-lambda [(x) y])))
           (define c%
             (class object%
               (super-new)
               (: m (case-> (Any -> Void)))
               (public m)
               (define m (case-lambda [(x) (void)]))))
           (send (new c%) m 'anything))
         -Void]
   ;; fails, test that case-lambda bodies are checked
   [tc-err (class object%
             (super-new)
             (: m (case-> (Any -> Integer)))
             (public m)
             (define m (case-lambda [(x) "bad"])))
           #:ret (tc-ret (-class #:method [(m (t:-> Univ -Integer))]))
           #:msg #rx"expected: Integer.*given: String"]
   ;; test that rest args work
   [tc-e (let ()
           (class object% (super-new)
             (: m (-> Integer * Integer))
             (define/public (m . xs) (apply + xs)))
           (void))
         -Void]
   ;; test that Name types are ok with get-field and as an
   ;; expected type for class type-checking
   [tc-e (let ()
           (define-type-alias Foo%
             (Class (init-field [x String])
                                [m (-> (Instance Foo%) String)]))
           (: foo% Foo%)
           (define foo%
             (class object% (super-new)
               (init-field x)
               (define/public (m a-foo) (get-field x a-foo))))
           (void))
         -Void]
   ;; test that propositions are correctly handled for polymorphic classes
   [tc-e (let ()
           (class object%
             (super-new)
             (init x)))
         #:ret (tc-ret (-poly (A) (-class #:init ([x A #f]))))
         #:expected (tc-ret (-poly (A) (-class #:init ([x A #f]))) #f #f)]
   ;; test uses of a macro in the body of the class
   [tc-e
    (let ()
      (define c%
        (class object%
          (super-new)
          (define-syntax-rule (my-meth (m arg ...) . body)
            (define/public (m arg ...) . body))
          (my-meth (hello) (displayln "hello world"))))
      (send (new c%) hello))
    -Void]
   ;; the next few tests check failing class instantiation
   [tc-err (make-object object% 1)
           #:msg #rx"expected: 0 arguments.*given: 1 arguments"]
   [tc-err (make-object (ann object% ClassTop))
           #:msg #rx"cannot instantiate.*ClassTop"]
   [tc-err (make-object 3)
           #:msg #rx"value of a non-class type"]
   ;; PR 14726
   ;; test opt-arg but non-keyword method
   [tc-e (let ()
           (define-type-alias A%
             (Class [foo (->* [Integer] Void)]))
           (: a% A%)
           (define a%
             (class object%
               (super-new)
               (define/public (foo [i #f]) (void))))
           (new a%))
         (-object #:method ([foo (t:-> -Integer -Void)]))]
   [tc-e (let ()
           (define-type-alias A%
             (Class [foo (->* [] [Integer] Void)]))
           (: a% A%)
           (define a%
             (class object%
               (super-new)
               (define/public (foo [i #f]) (void))))
           (new a%))
         (-object #:method ([foo (cl->* (t:-> -Void) (t:-> -Integer -Void))]))]
   ;; PR 14810 - make sure inner type mapping has the right order
   [tc-e (let ()
           (define-type-alias Foo%
             (Class [m (-> Any Symbol)]
                    [o (-> Any Any Symbol)]
                    (augment [m (-> Any Symbol)]
                             [o (-> Any Any Symbol)])))
           (define-type-alias Bar% (Class #:implements Foo%))
           (: foo% Foo%)
           (define foo%
             (class object%
               (super-new)
               (define/pubment (m x) 'foo-m)
               (define/pubment (o x y) 'foo-o)))
           (: bar% Bar%)
           (define bar%
             (class foo%
               (super-new)
               (define/augment (m x) (inner 'bar-m m x))
               (define/augment (o x y) (inner 'bar-o o x y))))
           (void))
         -Void]
   ;; Test implements clauses
   [tc-e (let ()
           (define-type-alias A% (Class [foo (-> Void)]))
           (define-type-alias B% (Class #:implements A% [bar (-> Void)]))
           (: b% B%)
           (define b% (class object%
                        (super-new)
                        (define/public (foo) (void))
                        (define/public (bar) (void))))
           (new b%))
         (-object #:method ([foo (t:-> -Void)] [bar (t:-> -Void)]))]
   [tc-err (let ()
             (define-type-alias A% (Class [foo (-> Void)]))
             (define-type-alias B% (Class #:implements A% [bar (-> Void)]))
             (: b% B%)
             (define b% (class object%
                          (super-new)
                          (define/public (bar) (void))))
             (error "foo"))
           #:msg "lacks expected method `foo'"]
   [tc-e (let ()
           (define-type-alias A% (Class (init [y Symbol])))
           (define-type-alias B% (Class #:implements/inits A% (init [x String])))
           (: b% B%)
           (define b% (class object%
                        (super-new)
                        (init x y)))
           (make-object b% "foo" 'bar)
           (void))
        -Void]
   [tc-e (let ()
           (define-type-alias A% (Class (init [y Symbol])))
           (define-type-alias B% (Class [m (-> Void)]))
           (define-type-alias C% (Class #:implements/inits A%
                                        #:implements B%
                                        (init [x String])))
           (: c% C%)
           (define c% (class object%
                        (super-new)
                        (define/public (m) (void))
                        (init x y)))
           (make-object c% "foo" 'bar)
           (void))
        -Void]
   [tc-err (let ()
             (define-type-alias A% (Class (init [y String])))
             (define-type-alias B% (Class #:implements/inits A%
                                          #:implements/inits A%))
             (error "foo"))
           ;; FIXME: this error message is pretty bad
           #:msg "expected Class type clause"]
   [tc-err (let ()
             (define-type-alias A% (Class (init [y String])))
             (define-type-alias B% (Class #:implements/inits A% (init [x String])))
             (: b% B%)
             (define b% (class object%
                          (super-new)
                          (init y x)))
             (error "foo"))
           #:msg #rx"initialization argument order.*expected: \\(x y\\).*given: \\(y x\\)"]
   ;; PR 14669 (next two)
   [tc-e (let ()
           (define-type-alias A (Class [m (-> Any)]))
           (define-type-alias B (Class #:implements A [m (-> Void)]))
           (define-type-alias C (Class #:implements A))
           (define-type-alias D (Class #:implements C #:implements B))
           (: d% D)
           (define d% (class object% (super-new) (define/public (m) (void))))
           (send (new d%) m))
         -Void]
   [tc-e (let ()
           (define-type-alias A (Class [m (-> Any)]))
           (define-type-alias B (Class #:implements A [m (-> Void)]))
           (define-type-alias C (Class #:implements A))
           (define-type-alias D (Class #:implements B #:implements C))
           (: d% D)
           (define d% (class object% (super-new) (define/public (m) (void))))
           (send (new d%) m))
         Univ]
   ;; Test for an infinite loop bug during subtyping for classes
   [tc-e (let ()
           (define-type-alias A% (Class [get-this (-> (Instance A%))]))
           (define-type-alias B% (Class [get-this (-> (Instance B%))]))
           (: a% A%)
           (define a%
             (class object%
               (super-new)
               (define/public (get-this) this)))
           (: b% B%)
           (define b%
             (class a%
               (super-new)
               (define/override (get-this) this)))
           (void))
         -Void]
   ;; Test that depth subtyping is accounted for with overriden methods
   [tc-e (let ()
           (define-type-alias B% (Class [n (-> Real)]))
           (: b% B%)
           (define b%
             (class object%
               (super-new)
               (define/public (n) 123.456)))
           (define-type-alias C% (Class #:implements B% [n (-> Integer)]))
           (: c% C%)
           (define c%
             (class b%
               (super-new)
               (override* [n (lambda () 5)])))
           (send (new c%) n))
         -Integer]
   ;; PR 14904
   [tc-e (class object%
           (super-new)
           (: foo (All (X) (-> X X)))
           (define/public foo (tr:lambda #:forall (A) ([x : A]) x)))
         (-class #:method [(foo (-poly (a) (t:-> a a)))])]
   [tc-e (class object%
           (super-new)
           (define/public foo (case-lambda [(str) (void)] [(sym size) (void)])))
         (-class #:method [(foo (cl->* (t:-> Univ Univ -Void : -true-propset) (t:-> Univ -Void : -true-propset)))])]
   ;; PR 14911
   [tc-e (class object%
           (super-new)
           (: bar (-> String String))
           (define bar (lambda (x) x))
           (bar "foo"))
         (-class)]
   ;; The next several tests are for check-below error messages for checking
   ;; expected types that are Class types
   [tc-err (let ()
             (: f (All (X #:row) (-> (Class #:row-var X) (Class #:row-var X))))
             (define (f cls) (class object% (super-new)))
             (error "foo"))
           #:msg #rx"expected: Class with row variable `X.*given: Class with no row variable"]
   [tc-err (let ()
             (: f (All (X #:row) (-> (Class #:row-var X) (Class))))
             (define (f cls) cls)
             (error "foo"))
           #:msg #rx"expected: Class with no row variable.*given: Class with row variable `X"]
   [tc-err (let ()
             (ann (class object% (super-new))
                  (Class (init [x String])))
             (error "foo"))
           #:msg #rx"lacks expected init `x'"]
   [tc-err (let ()
             (ann (class object% (super-new))
                  (Class (field [x String])))
             (error "foo"))
           #:msg #rx"lacks expected field `x'"]
   [tc-err (let ()
             (ann (class object% (super-new))
                  (Class [m (-> String)]))
             (error "foo"))
           #:msg #rx"lacks expected method `m'"]
   [tc-err (let ()
             (ann (class object% (super-new) (init [x : Symbol]))
                  (Class (init [x String])))
             (error "foo"))
           #:msg #rx"expected: String.*given: Symbol"]
   [tc-err (let ()
             (ann (class object% (super-new) (init [x : String "x"]))
                  (Class (init [x String])))
             (error "foo"))
           #:msg #rx"expected: mandatory init `x'.*given: optional init `x'"]
   [tc-err (let ()
             (ann (class object% (super-new) (init [x : String]))
                  (Class (init [x String #:optional])))
             (error "foo"))
           #:msg #rx"expected: optional init `x'.*given: mandatory init `x'"]
   [tc-err (let ()
             (ann (class object% (super-new) (field [x : Symbol 'x]))
                  (Class (field [x String])))
             (error "foo"))
           #:msg #rx"expected: String.*given: Symbol"]
   [tc-err (let ()
             (define c% (class object% (super-new) (define/public (m) (void))))
             (ann c% (Class [m (-> String)]))
             (error "foo"))
           #:msg #rx"expected: \\(-> String\\).*given: \\(-> Void\\)"]
   [tc-err (let ()
             (ann (class object% (super-new) (init-rest [rst : (Listof String)]))
                  (Class))
             (error "foo"))
           #:msg #rx"expected: Class with no init-rest type.*given: Class with init-rest type"]
   [tc-err (let ()
             (ann (class object% (super-new))
                  (Class [init-rest (Listof Void)]))
             (error "foo"))
           #:msg #rx"expected: Class with init-rest type.*given: Class with no init-rest type"]
   [tc-err (let ()
             (ann (class object% (super-new) (init-rest [rst : (Listof String)]))
                  (Class (init-rest (Listof Void))))
             (error "foo"))
           #:msg #rx"expected: \\(Listof Void\\).*given: \\(Listof String\\)"]
   ;; PR 14942
   [tc-err (let ()
             (: x (Instance BadAlias))
             (define x (error "foo"))
             (: y (Object (field [x Any])))
             (define y x)
             (error "foo"))
           #:msg "parse error in type"]
   ;; Test type aliases inside class bodies
   [tc-e (class object%
           (super-new)
           (define-type-alias X String)
           (: x X)
           (define x "foo"))
         (-class)]
   [tc-e (class object%
           (super-new)
           (define-type-alias (F X) (Listof X))
           (: x (F String))
           (define x (list "foo")))
         (-class)]
   [tc-err (let ()
             (class object%
               (super-new)
               (define-type-alias X String)
               (: x X)
               (define x "foo"))
             (: x2 X)
             (define x2 "bar")
             (error "foo"))
           #:msg "type name `X' is unbound"]
   [tc-e (let ()
           (class object%
             (super-new)
             (define-type-alias X String)
             (: x X)
             (define x "foo"))
           (define-type-alias X Symbol)
           (: x2 X)
           (define x2 'bar)
           (void))
         -Void]
   ;; Check strange method definition forms. Some of these are unlikely to actually come
   ;; up but are allowed by the grammar of classes.
   [tc-e (let ()
           (define c%
             (class object%
               (super-new)
               (public m)
               (define-values (m)
                 (let-values ([(x) (lambda () (void))]
                              [(y) (lambda () (void))])
                   (let-values ([(z) (lambda () (void))])
                     z)))))
           (send (new c%) m))
         -Void]
   [tc-e (let ()
           (define c%
             (class object%
               (super-new)
               (public m)
               (define-values (m)
                 (let-values ([(x) (lambda () (void))])
                   (let-values ([(y) (lambda () (void))])
                     (lambda () (y) (x)))))))
           (send (new c%) m))
         -Void]
   [tc-e (let ()
           (define c%
             (class object%
               (super-new)
               (public m)
               (define-values (m)
                 (letrec-values ([(x) (lambda () (void))])
                   (letrec-values ([(y) (lambda () (void))])
                     (lambda () (y) (x)))))))
           (send (new c%) m))
         -Void]
   [tc-e (let ()
           (define c%
             (class object%
               (super-new)
               (public m)
               (define-values (m)
                 (let-values ([(x) (lambda () (void))])
                   (let-values ([(y) (lambda () (void))])
                     (case-lambda [() (void)]))))))
           (send (new c%) m))
         -Void]
   [tc-e (let ()
           (define c%
             (class object%
               (super-new)
               (public m)
               (define-values (m)
                 (let-values ([(x) (lambda () (void))])
                   (let-values ([(y) (lambda () (void))])
                     (case-lambda [() (x)]))))))
           (send (new c%) m))
         -Void]
   [tc-e (let ()
           (define c%
             (class object%
               (super-new)
               (public m)
               (define-values (m)
                 (let-values ([(x) (lambda () (void))])
                   (let-values ([(y) (case-lambda [() (x)])])
                     y)))))
           (send (new c%) m))
         -Void]
   [tc-e (let ()
           (define c%
             (class object%
               (super-new)
               (public m)
               (define-values (m)
                 (let-values ([(x) (lambda () (void))])
                   (let-values ([(y) (case-lambda [() (x)])])
                     (tr:lambda (#:x [x "x"]) (void)))))))
           (send (new c%) m))
         -Void]
   [tc-e (let ()
           (define c%
             (class object% (super-new)
               (: m (-> Integer #:x Integer Integer))
               (public m)
               (define-values (m)
                 (let-values ([(m) (tr:lambda (x #:x y) (add1 y))]) m))))
           (send (new c%) m 0 #:x 1))
         -Integer]
   ;; This tests a bug that came up while adding support for the test
   ;; cases directly above
   [tc-e (let ()
           (define c%
             (class object% (super-new)
               (define/public (m [x : Symbol 'y])
                 (symbol->string x) (void))))
           (send (new c%) m))
         -Void]
   ;; Next several tests are for occurrence typing on private fields
   [tc-e (let ()
           (define c%
             (class object%
               (super-new)
               (: x (U String #f))
               (define x "foo")
               (: m (-> String))
               (define/public (m)
                 (if (string? x) (string-append x "bar") "baz"))))
           (send (new c%) m))
         -String]
   [tc-e (let ()
           (define c%
             (class object%
               (super-new)
               (: x (U String #f))
               (define x "foo")
               (: m (-> String))
               (define/public (m)
                 ;; ensure just x works
                 (if x (string-append x "bar") "baz"))))
           (send (new c%) m))
         -String]
   [tc-e (let ()
           (define c%
             (class object%
               (super-new)
               (: x (List (U String #f)))
               (define x (list "foo"))
               (: m (-> String))
               (define/public (m)
                 (if (string? (car x)) ; car path
                     (string-append (car x) "bar")
                     "baz"))))
           (send (new c%) m))
         -String]
   [tc-e (class object%
           (super-new)
           (: x (Option String))
           (define x "foo")
           ;; let-aliasing + occ. typing on fields
           (let ([y x]) (if (string? y) (string-append x) "")))
         (-class)]
   [tc-e (class object%
           (super-new)
           (: x (Option String))
           (define x "foo")
           (let ([y x]) (if y (string-append x) "")))
         (-class)]
   ;; Failure tests for occurrence typing on private fields. The types
   ;; are obfuscated a bit to prevent interference from type aliases in
   ;; another test.
   [tc-err (let ()
             (define c%
               (class object%
                 (super-new)
                 (: x (U String 'obfuscate))
                 (define x "foo")
                 (set! x 'obfuscate) ; prevents occ. typing
                 (: m (-> String))
                 (define/public (m)
                   (if (string? x) (string-append x "bar") "baz"))))
             (error "foo"))
           #:msg #rx"expected: String.*given: \\(U String 'obfuscate\\)"]
   [tc-err (let ()
             (define c%
               (class object%
                 (super-new)
                 (: x (U String 'obfuscate))
                 (define x "foo")
                 (field [f (begin (set! x 'obfuscate) "hello")])
                 (: m (-> String))
                 (define/public (m)
                   (if (string? x) (string-append x "bar") "baz"))))
             (error "foo"))
           #:msg #rx"expected: String.*given: \\(U String 'obfuscate\\)"]
   [tc-err (let ()
             (define c%
               (class object%
                 (super-new)
                 (: x (U String 'obfuscate))
                 (define x "foo")
                 (define/public (n) (set! x 'obfuscate))
                 (: m (-> String))
                 (define/public (m)
                   (if (string? x) (string-append x "bar") "baz"))))
             (error "foo"))
           #:msg #rx"expected: String.*given: \\(U String 'obfuscate\\)"]
   ;; tests that we are not creating objects for mutable private fields
   [tc-e (let ()
           (class object%
             (super-new)
             (: bsp-trees (U #f Integer))
             (define bsp-trees #f)
             (: m (-> Any))
             (define (m) (set! bsp-trees 5))
             
             (: sync-bsp-trees (-> Integer))
             (define/private (sync-bsp-trees)
               (let ([bsp-trees-val bsp-trees])
                 (cond
                   [bsp-trees-val  bsp-trees-val]
                   [else 5]))))
           (void))
         -Void]
  ;; tests private fields declared with define-values
  [tc-e (let ()
          (send
            (new
              (class object%
                (super-new)
                (define-values (a b) (values 1 "foo"))
                (: get-ab (-> (Values Integer String)))
                (define/public (get-ab) (values a b))))
            get-ab)
          (void))
        -Void]
  [tc-e (let ()
          (send
            (new
              (class object%
                (super-new)
                (define-values (a b)
                  (let ([x 1] [y "foo"]) (values x y)))
                (: get-ab (-> (Values Integer String)))
                (define/public (get-ab) (values a b))))
            get-ab)
          (void))
        -Void]
  ;; Failure tests for soundness of private field initialization
  [tc-err (let ()
            (define c%
              (class object%
                (super-new)
                (: a String)
                (define-values (a b) (values 1 2))
                (: get-a (-> String))
                (define/public (get-a) a)))
            (error "foo"))
          #:msg #rx"expected: String.*given: One"]
  [tc-err (let ()
            (define c%
              (class object%
                (super-new)
                (: a String)
                (define-values (a b) (let ([z 1]) (values z z)))
                (: get-a (-> String))
                (define/public (get-a) a)))
            (error "foo"))
          #:msg #rx"expected: String.*given: One"]
  ;; Make sure `send` works on a recursively typed object
  [tc-e (let ()
          (: o (Rec X (Object [m (-> Void)] [n (-> X Void)])))
          (define o
            (make-object (class object%
                           (super-new)
                           (define/public (m) (void))
                           (define/public (n x) (void)))))
          (send o m))
        -Void]
  ;; A test for GH issue #218. Make sure that multiple private fields
  ;; are typechecked in the right context.
  [tc-e (let ()
          (define-type-alias C%
            (Class (init-field (path Path-String))))
          (: c% C%)
          (define c%
            (class object%
              (init-field path)
              (: in Input-Port)
              (: out Output-Port)
              (define-values (in out)
                (values (open-input-file path) (open-output-file path)))
              (super-new)))
          (void))
        -Void]))
