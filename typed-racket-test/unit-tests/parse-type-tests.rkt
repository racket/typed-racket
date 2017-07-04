#lang racket/base
(require "test-utils.rkt"
         "evaluator.rkt"
         (for-syntax
           racket/base
           racket/dict
           racket/set
           racket/list
           syntax/parse
           (base-env base-structs)
           (env tvar-env type-alias-env mvar-env)
           (utils tc-utils)
           (private parse-type)
           (rep type-rep values-rep)

           (submod typed-racket/base-env/base-types initialize)
           (rename-in (types abbrev numeric-tower resolve prop-ops)
                      [Un t:Un] [-> t:->] [->* t:->*]))
         (only-in typed-racket/typed-racket do-standard-inits)
         (base-env base-types base-types-extra colon)
         ;; needed for parsing case-lambda/case-> types
         (only-in (base-env case-lambda) case-lambda)
         (prefix-in un: (only-in racket/class init init-field field augment))
         (only-in typed/racket/class init init-field field augment)
         (only-in racket/unit import export init-depend)

         rackunit)

(provide tests)
(gen-test-main)


(define mutated-var #f)
(define not-mutated-var #f)

(define x #'x)
(define y #'y)
(define z #'z)

(begin-for-syntax
  (do-standard-inits)
  (register-mutated-var #'mutated-var))

(define-syntax (pt-test stx)
  (syntax-parse stx
    [(_ (~datum FAIL) ty-stx:expr
        (~optional tvar-env:expr #:defaults [(tvar-env #'initial-tvar-env)])
        (~optional (~seq #:msg msg*:expr) #:defaults [(msg* #'#f)]))
     (quasisyntax/loc stx
       (test-case #,(format "~a" (syntax->datum #'ty-stx))
         (define msg msg*)
         (define actual-message
           (phase1-phase0-eval
             (with-handlers ([exn:fail:syntax? (lambda (exn) #`#,(exn-message exn))])
               (parameterize ([current-tvars tvar-env]
                              [delay-errors? #f])
                 (parse-type (quote-syntax ty-stx)))
               #'#f)))
         (unless actual-message
           (fail-check "No syntax error when parsing type."))
         (when msg
           (unless (regexp-match? msg actual-message)
             (with-check-info (['expected msg] ['actual actual-message])
               (fail-check "parse-type raised the wrong error message"))))))]
    [(_ ty-stx:expr ty-val:expr
        (~optional tvar-env:expr #:defaults [(tvar-env #'initial-tvar-env)]))
     (quasisyntax/loc
       stx
       (test-case #,(format "~a" (syntax->datum #'ty-stx))
         (define-values (expected actual same?)
           (phase1-phase0-eval
             (parameterize ([current-tvars tvar-env]
                            [delay-errors? #f])
                (define expected ty-val)
                (define actual (parse-type (quote-syntax ty-stx)))
                #`(values #,expected #,actual #,(equal? actual expected)))))
          (unless same?
            (with-check-info (['expected expected] ['actual actual])
              (fail-check "Unequal types")))))]))

(define-syntax pt-tests
  (syntax-rules ()
    [(_ nm [elems ...] ...)
     (test-suite nm
                 (pt-test elems ...) ...)]))

(define-for-syntax N -Number)
(define-for-syntax B -Boolean)
(define-for-syntax Sym -Symbol)

(define tests
  (pt-tests
   "parse-type tests"
   [FAIL UNBOUND]
   [FAIL List]
   [FAIL (All (A) (List -> Boolean))]
   [Number N]
   [Any Univ]
   [(List Number String) (-Tuple (list N -String))]
   [(All (Number) Number) (-poly (a) a)]
   [(Number . Number) (-pair N N)]
   [(Listof Boolean) (make-Listof  B)]
   [(Vectorof (Listof Symbol)) (make-Vector (make-Listof Sym))]
   [(Immutable-Vectorof (Listof Symbol)) (make-Immutable-Vector (make-Listof Sym))]
   [(Mutable-Vectorof (Listof Symbol)) (make-Mutable-Vector (make-Listof Sym))]
   [(Vector Symbol String) (make-HeterogeneousVector (list Sym -String))]
   [(Immutable-Vector Symbol String) (make-Immutable-HeterogeneousVector (list Sym -String))]
   [(Mutable-Vector Symbol String) (make-Mutable-HeterogeneousVector (list Sym -String))]
   [(pred Number) (make-pred-ty N)]
   [(-> (values Number Boolean Number)) (t:-> (-values (list N B N)))]
   [(Number -> Number) (t:-> N N)]
   [(All (A) Number -> Number) (-poly (a) (t:-> N N))]
   [(All (A) Number -> Number -> Number) (-poly (a) (t:-> N (t:-> N N)))]
   [(All (A) Number -> Number -> Number -> Number)
    (-poly (a) (t:-> N (t:-> N (t:-> N N))))]
   [FAIL (All (A) -> Number Number)]
   [FAIL (All (A) Listof Any)]
   [(All (A) (Number -> Number)) (-poly (a) (t:-> N N))]
   [(All (A) (-> Number Number)) (-poly (a) (t:-> N N))]
   [(All (A) A -> A) (-poly (a) (t:-> a a))]
   [(All (A) A → A) (-poly (a) (t:-> a a))]
   [FAIL (All (A) → A A)]
   [(All (A) (A -> A)) (-poly (a) (t:-> a a))]
   [(All (A) (-> A A)) (-poly (a) (t:-> a a))]
   [FAIL (All (A) -> Integer -> Integer -> Integer)]
   ;; requires transformer time stuff that doesn't work
   #;[(Refinement even?) (make-Refinement #'even?)]
   [(Number Number Number Boolean -> Number) (N N N B . t:-> . N)]
   [(-> Number Number Number Boolean Number) (N N N B . t:-> . N)]
   [(Number Number Number * -> Boolean) ((list N N) N . t:->* . B)]
   [(-> Number Number Number * Boolean) ((list N N) N . t:->* . B)]
   ;[((. Number) -> Number) (->* (list) N N)] ;; not legal syntax
   [(U Number Boolean) (t:Un N B)]
   [(Union Number Boolean) (t:Un N B)]
   [(U Number Boolean Number) (t:Un N B)]
   [(U Number Boolean 1) (t:Un N B)]
   [(All (a) (Listof a)) (-poly (a) (make-Listof  a))]
   [(All (a ...) (a ... a -> Integer)) (-polydots (a) ( (list) (a a) . ->... . -Integer))]
   [(All (a ...) (-> a ... a Integer)) (-polydots (a) ( (list) (a a) . ->... . -Integer))]
   [(∀ (a) (Listof a)) (-poly (a) (make-Listof  a))]
   [(∀ (a ...) (a ... a -> Integer)) (-polydots (a) ( (list) (a a) . ->... . -Integer))]
   [(∀ (a ...) (-> a ... a Integer)) (-polydots (a) ( (list) (a a) . ->... . -Integer))]
   [(All (a ...) (a ... -> Number))
    (-polydots (a) ((list) [a a] . ->... . N))]
   [(All (a ...) (-> a ... Number))
    (-polydots (a) ((list) [a a] . ->... . N))]
   [(All (a ...) (-> (values a ...)))
    (-polydots (a) (t:-> (make-ValuesDots (list) a 'a)))]
   [(-> Number AnyValues) (t:-> N ManyUniv)]

   ;; PR 14554, non-productive recursive type
   [FAIL (Rec x (All (A #:row) x))]

   [(case-lambda (Number -> Boolean) (Number Number -> Number)) (cl-> [(N) B]
                                                                      [(N N) N])]
   [(case-> (Number -> Boolean) (Number Number -> Number)) (cl-> [(N) B]
                                                                 [(N N) N])]
   [(case-> (Number -> Boolean) (-> Number Number Number)) (cl-> [(N) B]
                                                                 [(N N) N])]
   [(case-> (Boolean -> Boolean)
            (-> Boolean Boolean Boolean)
            (-> Boolean String * Boolean)
            (->* (Boolean) #:rest String Boolean)
            (->* (Boolean) #:rest-star (String Symbol) Boolean)
            (->* (Boolean) (Boolean) #:rest-star (String Symbol) Boolean)
            (->* (Boolean Boolean) #:rest-star (String Symbol) Boolean))
    (make-Fun
     (remove-duplicates
      (list (-Arrow (list -Boolean) -Boolean)
            (-Arrow (list -Boolean -Boolean) -Boolean)
            (-Arrow (list -Boolean) #:rest -String -Boolean)
            (-Arrow (list -Boolean)
                    #:rest (make-Rest (list -String -Symbol))
                    -Boolean)
            (-Arrow (list -Boolean -Boolean)
                    #:rest (make-Rest (list -String -Symbol))
                    -Boolean))))]
   [1 (-val 1)]
   [#t (-val #t)]
   [#f (-val #f)]
   ["foo" (-val "foo")]
   ['(1 2 3) (-Tuple (map -val '(1 2 3)))]

   [(Listof Number) (make-Listof  N)]

   [a (-v a) (dict-set initial-tvar-env 'a (-v a))]

   [(Any -> Boolean : Number) (make-pred-ty -Number)]
   [(-> Any Boolean : Number) (make-pred-ty -Number)]
   [(Any -> Boolean : #:+ (Number @ 0) #:- (! Number @ 0))
    (make-pred-ty -Number)]
   [(-> Any Boolean : #:+ (Number @ 0) #:- (! Number @ 0))
    (make-pred-ty -Number)]
   [(Any -> Boolean : #:+ (! Number @ 0) #:- (Number @ 0))
    (t:->* (list Univ) -Boolean : (-PS (-not-type 0 -Number) (-is-type 0 -Number)))]
   [(-> Any Boolean : #:+ (! Number @ 0) #:- (Number @ 0))
    (t:->* (list Univ) -Boolean : (-PS (-not-type 0 -Number) (-is-type 0 -Number)))]
   [(-> Any (-> Any Boolean : #:+ (Number @ 1 0) #:- (! Number @ 1 0)))
    (t:-> Univ
          (t:->* (list Univ) -Boolean : (-PS (-is-type (cons 1 0) -Number) (-not-type (cons 1 0) -Number))))]
   [(-> Any Any (-> Any Boolean : #:+ (Number @ 1 1) #:- (! Number @ 1 1)))
    (t:-> Univ Univ
          (t:->* (list Univ) -Boolean : (-PS (-is-type (cons 1 1) -Number) (-not-type (cons 1 1) -Number))))]
   [(-> Any #:foo Any (-> Any Boolean : #:+ (Number @ 1 0) #:- (! Number @ 1 0)))
    (->key Univ #:foo Univ #t
           (t:->* (list Univ) -Boolean : (-PS (-is-type (cons 1 0) -Number) (-not-type (cons 1 0) -Number))))]
   [(All (a b) (-> (-> a Any : #:+ b) (Listof a) (Listof b)))
    (-poly (a b) (t:-> (asym-pred a Univ (-PS (-is-type 0 b) -tt)) (-lst a) (-lst b)))]
   [(All (a b) (-> (-> a Any : #:+ (! b)) (Listof a) (Listof b)))
    (-poly (a b) (t:-> (asym-pred a Univ (-PS (-not-type 0 b) -tt)) (-lst a) (-lst b)))]
   [(All (a b) (-> (-> a Any : #:- b) (Listof a) (Listof b)))
    (-poly (a b) (t:-> (asym-pred a Univ (-PS -tt (-is-type 0 b))) (-lst a) (-lst b)))]
   [(All (a b) (-> (-> a Any : #:- (! b)) (Listof a) (Listof b)))
    (-poly (a b) (t:-> (asym-pred a Univ (-PS -tt (-not-type 0 b))) (-lst a) (-lst b)))]
   [(Number -> Number -> Number)
    (t:-> -Number (t:-> -Number -Number))]
   [(-> Number (-> Number Number))
    (t:-> -Number (t:-> -Number -Number))]
   [(Integer -> (All (X) (X -> X)))
    (t:-> -Integer (-poly (x) (t:-> x x)))]
   [(-> Integer (All (X) (-> X X)))
    (t:-> -Integer (-poly (x) (t:-> x x)))]
   [FAIL -> #:msg "incorrect use of -> type constructor"]
   [FAIL (Any -> Any #:object 0) #:msg "expected the identifier `:'"]
   [FAIL (-> Any Any #:+ (String @ x)) #:msg "expected the identifier `:'"]
   [FAIL (-> Any Boolean : #:+ (Number @ 1 0) #:- (! Number @ 1 0))
         #:msg "Index 1 used in"]
   [FAIL (-> Any (-> Any Boolean : #:+ (Number @ 1 1) #:- (! Number @ 1 1)))
         #:msg "larger than argument length"]


   [(Any -> Boolean : #:+ (Symbol @ not-mutated-var))
    (t:-> Univ -Boolean : (-PS (-is-type (-id-path #'not-mutated-var) -Symbol) -tt))]
   [FAIL (Any -> Boolean : #:+ (Symbol @ mutated-var))
         #:msg "may not reference identifiers that are mutated"]
   [(Any -> Boolean : #:+ (! Symbol @ not-mutated-var))
    (t:-> Univ -Boolean : (-PS (-not-type (-id-path #'not-mutated-var) -Symbol) -tt))]
   [FAIL (Any -> Boolean : #:+ (! Symbol @ mutated-var))
         #:msg "may not reference identifiers that are mutated"]
   [FAIL (Any -> Boolean : #:+ (String @ unbound))
         #:msg "may not reference identifiers that are unbound"]


   ;; ->* types
   [(->* (String Symbol) Void)
    (make-Fun (list (-Arrow (list -String -Symbol) -Void)))]
   [(->* () (String) #:rest Symbol Void)
    (make-Fun (list (-Arrow (list) -Void)
                    (-Arrow (list -String)
                            #:rest -Symbol
                            -Void)))]
   [(->* (Number) (String) #:rest Symbol Void)
    (make-Fun (list (-Arrow (list -Number) -Void)
                    (-Arrow (list -Number -String)
                            #:rest -Symbol
                            -Void)))]
   [(->* (Number) (String Void) #:rest Symbol Any)
    (make-Fun (list (-Arrow (list -Number) Univ)
                    (-Arrow (list -Number -String) Univ)
                    (-Arrow (list -Number -String -Void)
                            #:rest -Symbol
                            Univ)))]
   [(->* (String Symbol) (String) Void)
    (->opt -String -Symbol [-String] -Void)]
   [(->* (String Symbol) (String Symbol) Void)
    (->opt -String -Symbol [-String -Symbol] -Void)]
   [(->* (String Symbol) (String) (values Void String))
    (->opt -String -Symbol [-String] (-values (list -Void -String)))]
   [(->* (String Symbol) (String) #:rest Symbol Void)
    (->optkey -String -Symbol [-String] #:rest -Symbol -Void)]
   [(All (a) (->* (a Symbol) (String) #:rest Symbol Void))
    (-poly (a) (->optkey a -Symbol [-String] #:rest -Symbol -Void))]
   [(->* (Integer) (String #:foo Integer String) Void)
    (->optkey -Integer [-String -String] #:foo -Integer #f -Void)]
   [(->* (Integer) (String #:foo Integer) Void)
    (->optkey -Integer [-String] #:foo -Integer #f -Void)]
   [(->* (Integer) (#:foo Integer String) Void)
    (->optkey -Integer [-String] #:foo -Integer #f -Void)]
   [(->* (Integer #:bar Integer) (String) Void)
    (->optkey -Integer [-String] #:bar -Integer #t -Void)]
   [(->* (#:bar Integer Integer) (String) Void)
    (->optkey -Integer [-String] #:bar -Integer #t -Void)]
   [(->* (Integer #:bar Integer) (String #:foo Integer) Void)
    (->optkey -Integer [-String] #:bar -Integer #t #:foo -Integer #f -Void)]
   [(->* (#:bar Integer Integer) (#:foo Integer String) Void)
    (->optkey -Integer [-String] #:bar -Integer #t #:foo -Integer #f -Void)]
   [(->* (Any (-> Any Boolean : #:+ (String @ 1 0))) Void)
    (t:-> Univ (t:->* (list Univ) -Boolean : (-PS (-is-type (cons 1 0) -String) -tt))
          -Void)]
   [FAIL (->* (Any (-> Any Boolean : #:+ (String @ 2 0))) Void)
         #:msg "Index 2 used in"]

   [(Opaque foo?) (make-Opaque #'foo?)]
   ;; PR 14122
   [FAIL (Opaque 3)]

   ;; struct types
   [(Struct-Type arity-at-least) (make-StructType (resolve -Arity-At-Least))]
   [FAIL (Struct-Type Integer)]
   [FAIL (Struct-Type foo)]
   [Struct-TypeTop -StructTypeTop]

   ;; keyword function types
   [(#:a String -> String)
    (->optkey [] #:a -String #t -String)]
   [([#:a String] -> String)
    (->optkey [] #:a -String #f -String)]
   [(#:a String #:b String -> String)
    (->optkey [] #:a -String #t #:b -String #t -String)]
   [([#:a String] #:b String -> String)
    (->optkey [] #:a -String #f #:b -String #t -String)]
   [(#:a String [#:b String] -> String)
    (->optkey [] #:a -String #t #:b -String #f -String)]
   [(String #:a String -> String)
    (->optkey -String [] #:a -String #t -String)]
   [(String #:a String String * -> String)
    (->optkey -String [] #:rest -String #:a -String #t -String)]
   [(String [#:a String] String * -> String)
    (->optkey -String [] #:rest -String #:a -String #f -String)]

   ;; #:rest-star tests
   [(->* () #:rest-star () String)
    (->optkey () -String)]
   [(->* () (Symbol) #:rest-star (String Symbol) String)
    (->optkey (-Symbol) #:rest (make-Rest (list -String -Symbol)) -String)]
   [(->* () #:rest-star (String) String)
    (->optkey () #:rest (make-Rest (list -String)) -String)]
   [(->* () #:rest-star (String Symbol) String)
    (->optkey () #:rest (make-Rest (list -String -Symbol)) -String)]
   [(->* (String) #:rest-star (String Symbol) String)
    (->optkey -String () #:rest (make-Rest (list -String -Symbol)) -String)]
   [(->* (String) (Symbol) #:rest-star (String Symbol) String)
    (->optkey -String (-Symbol) #:rest (make-Rest (list -String -Symbol)) -String)]
   [(->* (String) (Symbol) #:rest-star () String)
    (->optkey -String (-Symbol) -String)]
   [FAIL (->* (String) #:rest-star Number String)]
   [FAIL (->* (String) (Symbol) #:rest-star Number String)]
   [FAIL (->* (String) (Symbol) #:rest-star (Not-A-Real-Type-Should-Fail) String)]

   ;;; Prefab structs
   [(Prefab foo String) (-prefab 'foo -String)]
   [FAIL (Prefab (foo 0) String)]

   ;;; Classes
   [(Class) (-class)]
   [(Class (init [x Number] [y Number]))
    (-class #:init ([x -Number #f] [y -Number #f]))]
   [(Class (un:init [x Number] [y Number]))
    (-class #:init ([x -Number #f] [y -Number #f]))]
   [(Class (init [x Number] [y Number #:optional]))
    (-class #:init ([x -Number #f] [y -Number #t]))]
   [(Class (init [x Number]) (init-field [y Number]))
    (-class #:init ([x -Number #f]) #:init-field ([y -Number #f]))]
   [(Class [m (Number -> Number)])
    (-class #:method ([m (t:-> N N)]))]
   [(Class [m (Number -> Number)] (init [x Number]))
    (-class #:init ([x -Number #f]) #:method ([m (t:-> N N)]))]
   [(Class [m (Number -> Number)] (field [x Number]))
    (-class #:field ([x -Number]) #:method ([m (t:-> N N)]))]
   [(Class [m (Number -> Number)] (un:field [x Number]))
    (-class #:field ([x -Number]) #:method ([m (t:-> N N)]))]
   [(Class (augment [m (Number -> Number)]))
    (-class #:augment ([m (t:-> N N)]))]
   [(Class (un:augment [m (Number -> Number)]))
    (-class #:augment ([m (t:-> N N)]))]
   [(Class (augment [m (Number -> Number)]) (field [x Number]))
    (-class #:augment ([m (t:-> N N)]) #:field ([x -Number]))]
   [(Class (augment [m (-> Number)]) [m (-> Number)])
    (-class #:method ([m (t:-> N)]) #:augment ([m (t:-> N)]))]
   [FAIL (Class foobar)]
   [FAIL (Class [x UNBOUND])]
   [FAIL (Class [x Number #:random-keyword])]
   [FAIL (Class (random-clause [x Number]))]
   [FAIL (Class [m Number])]
   [FAIL (Class (augment [m Number]))]
   ;; test duplicates
   [FAIL (Class [x Number] [x Number])]
   [FAIL (Class (init [x Number]) (init [x Number]))]
   [FAIL (Class (init [x Number]) (init-field [x Number]))]
   [FAIL (Class (field [x Number]) (init-field [x Number]))]
   [FAIL (Class (augment [m (-> Number)] [m (-> Number)]))]
   [FAIL (Class (augment [m (-> Number)]) (augment [m (-> Number)]))]
   [FAIL (Class [m (-> Number)] [m (-> Number)])]
   ;; test #:row-var
   [(All (r #:row) (Class #:row-var r))
    (make-PolyRow (list 'r)
                  (list null null null null)
                  (-class #:row (make-F 'r)))]
   [FAIL (All (r #:row) (Class #:implements (Class #:row-var r)))]
   [FAIL (All (r #:row) (Class #:implements (Class) #:row-var r))]
   [FAIL (Class #:row-var 5)]
   [FAIL (Class #:row-var (list 3))]
   [FAIL (Class #:row-var x)]
   [FAIL (Class #:implements (Class #:row-var r) #:row-var x)]
   [FAIL (Class #:implements (Class #:row-var r) #:row-var r)]
   [FAIL (All (r #:row)
           (All (x #:row)
            (Class #:implements (Class #:row-var r) #:row-var x)))]
   [FAIL (All (r #:row) (Class #:implements (Class #:row-var r) #:row-var r))]
   ;; Test #:implements, some of these used to work but now they have to
   ;; refer to type aliases. Testing actual type aliases is hard here though.
   [FAIL (Class #:implements (Class [m (Number -> Number)]) (field [x Number]))]
   [FAIL (Class #:implements (Class [m (Number -> Number)])
                #:implements (Class [n (Number -> Number)])
                (field [x Number]))]
   [FAIL (Class #:implements (Class [m (Number -> Number)])
                #:implements (Class [m (Number -> Number)])
                (field [x Number]))]
   [FAIL (Class #:implements (Class (init [x Integer]) [m (Number -> Number)])
                (field [x Number]))]
   [FAIL (Class #:implements Number)]
   [FAIL (Class #:implements Number [m (Number -> Number)])]
   [FAIL (Class #:implements (Class [m (Number -> Number)]) [m String])]
   [FAIL (Class #:implements (Class [m (Number -> Number)])
                #:implements (Class [m (String -> String)])
                (field [x Number]))]
   [FAIL (Class #:implements (Class (augment [m (Number -> Number)]))
                #:implements (Class (augment [m (String -> String)]))
                (field [x Number]))]
   [FAIL (Class #:implements (Class (augment [m (Number -> Number)]))
                (augment [m (-> Number)]))]
   ;; Test Object types
   [(Object) (-object)]
   [(Object [m (Number -> Number)])
    (-object #:method ([m (t:-> N N)]))]
   [(Object [m (Number -> Number)] (field [f Number]))
    (-object #:method ([m (t:-> N N)]) #:field ([f N]))]
   [FAIL (Object foobar)]
   [FAIL (Object [x UNBOUND])]
   [FAIL (Object [x Number #:random-keyword])]
   [FAIL (Object (random-clause [x Number]))]
   [FAIL (Object [x Number] [x Number])]
   [FAIL (Object (field [x Number]) (field [x Number]))]
   [FAIL (Object [x Number] [x Number])]
   [FAIL (Object [m Number])]
   ;; Test row polymorphic types
   [(All (r #:row) ((Class #:row-var r) -> (Class #:row-var r)))
    (-polyrow (r) (list null null null null)
      (t:-> (-class #:row r) (-class #:row r)))]
   [(Listof (All (r #:row) ((Class #:row-var r) -> (Class #:row-var r))))
    (-lst (-polyrow (r) (list null null null null)
                    (t:-> (-class #:row r) (-class #:row r))))]
   [(All (r #:row (init x y z) (field f) m n)
      ((Class #:row-var r) -> (Class #:row-var r)))
    (-polyrow (r) (list '(x y z) '(f) '(m n) '())
      (t:-> (-class #:row r) (-class #:row r)))]
   ;; Class types cannot use a row variable that doesn't constrain
   ;; all of its members to be absent in the row
   [FAIL (All (r #:row (init x))
           ((Class #:row-var r (init y)) -> (Class #:row-var r)))]
   [FAIL (All (r #:row (init x y z) (field f) m n)
           ((Class #:row-var r a b c) -> (Class #:row-var r)))]
   
   ;; parsing tests for Unit types
   ;; These are only simple tests because checking types
   ;; with signatures requires interaction with the Signature
   ;; environment. Additionally, more complex tests of Unit
   ;; type parsing happens in unit-tests and integrations tests as well
   [(Unit (import) (export) (init-depend) String)
    (make-Unit null null null (-values (list -String)))]
   [(Unit (import) (export) String)
    (make-Unit null null null (-values (list -String)))]
   [(Unit (import) (export) (init-depend))
    (make-Unit null null null (-values (list -Void)))]
   [(Unit (import) (export))
    (make-Unit null null null (-values (list -Void)))]
   [UnitTop -UnitTop]
   [FAIL (Unit (export) String)]
   [FAIL (Unit (import) String)]
   [FAIL (Unit (init-depend) String)]
   [FAIL (Unit (import bad) (export) String)]
   [FAIL (Unit (import) (export bad) String)]
   [(Sequenceof Any Any) (-seq Univ Univ)]

   ;; GH issue #314
   [FAIL ~> #:msg "unbound"]

   ;; intersections
   [(∩) Univ]
   [(∩ Any) Univ]
   [(∩ String Symbol) -Bottom]
   [(Intersection String Symbol) -Bottom]
   [(∩ (-> Number Number) (-> String String))
    (-unsafe-intersect (t:-> -String -String)
                       (t:-> -Number -Number))]
   ;; refinements
   ;; top/bot
   [(Refine [x : Number] Top) -Number]
   [(Refine [x : Number] Bot) -Bottom]
   ;; simplify props about subject
   [(Refine [x : Any] (: x String)) -String]
   [(Refine [x : Integer] (: x Integer)) -Int]
   [(Refine [x : Integer] (: x Symbol)) -Bottom]
   ;; refinements w/ inequalities
   [(Refine [val : Integer] (<= val 42))
    (-refine/fresh x -Int (-leq (-lexp x)
                                (-lexp 42)))]
   [(Refine [vec : (Vectorof Any)] (<= (vector-length vec) 42))
    (-refine/fresh x (-vec Univ) (-leq (-lexp (-vec-len-of (-id-path x)))
                                       (-lexp 42)))]
   [(Refine [p : (Pairof Integer Integer)] (<= (car p) (cdr p)))
    (-refine/fresh p (-pair -Int -Int) (-leq (-lexp (-car-of (-id-path p)))
                                             (-lexp (-cdr-of (-id-path p)))))]
   [(Refine [x : Integer] (<= (* 2 x) 42))
    (-refine/fresh x -Int (-leq (-lexp (list 2 x))
                                (-lexp 42)))]
   [(Refine [x : Integer] (<= (+ 1 x) 42))
    (-refine/fresh x -Int (-leq (-lexp 1 x)
                                (-lexp 42)))]
   [(Refine [x : Integer] (<= (- 1 x) 42))
    (-refine/fresh x -Int (-leq (-lexp 1 (-lexp (list -1 x)))
                                (-lexp 42)))]
   [(Refine [x : Integer] (<= (+ 1 (* 3 x)) 42))
    (-refine/fresh x -Int (-leq (-lexp 1 (list 3 x))
                                (-lexp 42)))]
   [(Refine [x : Integer] (<= (+ 1 (* 3 x) (* 2 x)) 42))
    (-refine/fresh x -Int (-leq (-lexp 1 (list 5 x))
                                (-lexp 42)))]
   [(Refine [x : Integer] (<= 42 (+ 1 (* 3 x) (* 2 x))))
    (-refine/fresh x -Int (-leq (-lexp 42)
                                (-lexp 1 (list 5 x))))]
   [(Refine [x : Integer] (<= 42 (* 2 x)))
    (-refine/fresh x -Int (-leq (-lexp 42)
                                (-lexp (list 2 x))))]
   [(Refine [x : Integer] (<= 42 (+ 1 x)))
    (-refine/fresh x -Int (-leq (-lexp 42)
                                (-lexp 1 x)))]
   [(Refine [x : Integer] (<= x 42))
    (-refine/fresh x -Int (-leq (-lexp x)
                                (-lexp 42)))]
   [(Refine [x : Integer] (< x 42))
    (-refine/fresh x -Int (-leq (-lexp 1 (list 1 x))
                                (-lexp 42)))]
   [(Refine [x : Integer] (>= x 42))
    (-refine/fresh x -Int (-leq (-lexp 42)
                                (-lexp x)))]
   [(Refine [x : Integer] (>= x 42))
    (-refine/fresh x -Int (-leq (-lexp 42)
                                (-lexp x)))]
   [(Refine [x : Integer] (> x 42))
    (-refine/fresh x -Int (-leq (-lexp 43)
                                (-lexp x)))]
   [(Refine [n : Integer] (<= (- (+ n n) (* 1 (+ n)))
                              (+ 2 (- 80 (* 2 (+ 9 9 (+) (-) 2))))))
    (-refine/fresh x -Int (-leq (-lexp x)
                                (-lexp 42)))]
   ;; id shadowing
   [(Refine [x : Any] (: x (Refine [x : Integer] (<= x 42))))
    (-refine/fresh x -Int (-leq (-lexp x)
                                (-lexp 42)))]
   ;; refinements w/ equality
   [(Refine [x : Integer] (= x 42))
    (-refine/fresh x -Int (-and (-leq (-lexp x) (-lexp 42))
                                (-leq (-lexp 42) (-lexp x))))]
   ;; other abritrary propositions in refinements
   [(Refine [x : Integer] (and (<= x 42)
                               (<= 0 x)))
    (-refine/fresh x -Int (-and (-leq (-lexp x) (-lexp 42))
                                (-leq (-lexp 0) (-lexp x))))]
   [(Refine [x : String] (and (: z Symbol)
                              (! y String)))
    (-refine/fresh x -String (-and (-is-type #'z -Symbol)
                                   (-not-type #'y -String)))]
   [(Refine [x : String] (or (: z Symbol)
                             (: y String)))
    (-refine/fresh x -String (-or (-is-type #'z -Symbol)
                                  (-is-type #'y -String)))]
   [(Refine [x : String] (unless (: z Symbol)
                           (: y String)))
    (-refine/fresh x -String (-or (-is-type #'z -Symbol)
                                  (-is-type #'y -String)))]
   [(Refine [x : String] (or (not (: y String))
                             (: z Symbol)))
    (-refine/fresh x -String (-or (-not-type #'y -String)
                                  (-is-type #'z -Symbol)))]
   [(Refine [x : Any] (if (: x String) (! y String) (: z Symbol)))
    (-refine/fresh x Univ (-or (-and (-is-type x -String) (-not-type #'y -String))
                               (-and (-not-type x -String) (-is-type #'z -Symbol))))]
   [(Refine [x : String] (when (: z Symbol) (: y String)))
    (-refine/fresh x -String (-or (-not-type #'z -Symbol)
                                  (-is-type #'y -String)))]
   [(Refine [x : String] (when (not (not (: z Symbol)))
                           (: y String)))
    (-refine/fresh x -String (-or (-not-type #'z -Symbol)
                                  (-is-type #'y -String)))]
   [(Refine [x : (Refine [x : Integer] (<= 42 x))] (<= x 42))
    (-refine/fresh z -Int (-and (-leq (-lexp 42) (-lexp z))
                                (-leq (-lexp z) (-lexp 42))))]
   ;; fail for unbound identifiers
   [FAIL (Refine [x : String] (: r Symbol))]
   [FAIL (Refine [x String] (: x Symbol))]
   [FAIL (Refine [x y : String] (: x Symbol))]
   [FAIL (Refine [x : String] (: r Symbol) (: r Symbol))]
   ;; fail for bad path element usage
   [FAIL (Refine [p : Integer] (<= (car p) 42))]
   [FAIL (Refine [p : Integer] (<= (cdr p) 42))]
   [FAIL (Refine [p : (Pairof Integer Integer)] (<= (car (car p)) 42))]
   [FAIL (Refine [p : (Pairof Integer Integer)] (<= (car (cdr p)) 42))]
   [FAIL (Refine [p : (Pairof Integer Integer)] (<= (cdr (car p)) 42))]
   [FAIL (Refine [p : (Pairof Integer Integer)] (<= (cdr (cdr p)) 42))]
   [FAIL (Refine [vec : Any] (<= (vector-length vec) 42))]
   ;; fail for bad linear expression (i.e. not an integer)
   [FAIL (Refine [q : Any] (<= q 42))]
   [FAIL (Refine [q : Any] (<= 42 q))]
   [FAIL (Refine [q : Any] (< q 42))]
   [FAIL (Refine [q : Any] (< 42 q))]
   [FAIL (Refine [q : Any] (>= 42 q))]
   [FAIL (Refine [q : Any] (>= q 42))]
   [FAIL (Refine [q : Any] (> q 42))]
   [FAIL (Refine [q : Any] (> 42 q))]
   [FAIL (Refine [q : Any] (= 42 q))]
   [FAIL (Refine [q : Any] (= q 42))]
   [FAIL (Refine [q : Any] (<= (+ 1 q) 42))]
   [FAIL (Refine [q : Any] (<= 42 (+ 1 q)))]
   [FAIL (Refine [q : Any] (<= (* 2 q) 42))]
   [FAIL (Refine [q : Any] (<= 42 (* 2 q)))]
   [FAIL (Refine [q : Any] (<= (+ 1 (* 2 q)) 42))]
   [FAIL (Refine [q : Any] (<= 42 (+ 1 (* 2 q))))]
   ;; id shadowing & bad linear expression
   [FAIL (Refine [x : Integer] (: x (Refine [x : Any] (<= 42 x))))]

   ;; dependent function syntax tests!
   ;; - - - - - - - - - - - - - - - - - - - -

   ;; no deps, no dep type!
   [(-> ([v : (Vectorof Any)])
        Any)
    (t:-> (-vec Univ) Univ)]
   [(-> ([v : (Vectorof Any)]
         [i : Integer])
        Any)
    (t:-> (-vec Univ) -Int Univ)]

   ;; if only dep range, still a DFun (if the type is dep)
   [(-> ([x : Integer]
         [y : Integer])
        (Refine [res : Integer] (<= res (+ x y))))
    (make-DepFun
     (list -Int -Int)
     -tt
     (-values
      (-refine/fresh res -Int
                     (-leq (-lexp (-id-path (cons 0 0)))
                           (-lexp (-id-path (cons 1 0))
                                  (-id-path (cons 1 1)))))))]
   ;; simple dep latent props/object (no dep type, no DFun)
   [(-> ([x : Any])
        Boolean
        #:+ (: x Integer))
    (t:-> Univ -Boolean : (-PS (-is-type (cons 0 0) -Int) -tt))]
   [(-> ([x : Any])
        Boolean
        #:- (! x Integer))
    (t:-> Univ -Boolean : (-PS -tt (-not-type (cons 0 0) -Int)))]
   [(-> ([x : Any])
        Boolean
        #:+ (: x Integer)
        #:- (! x Integer))
    (t:-> Univ -Boolean : (-PS (-is-type (cons 0 0) -Int)
                               (-not-type (cons 0 0) -Int)))]
   [(-> ([x : Any]
         [y : Any])
        Boolean
        #:+ (: x Integer)
        #:- (: y Integer)
        #:object x)
    (t:-> Univ Univ -Boolean
          : (-PS (-is-type (cons 0 0) -Int)
                 (-is-type (cons 0 1) -Int))
          : (-id-path (cons 0 0)))]
   ;; simple dependencies
   [(-> ([v : (Vectorof Any)]
         [i : (v) (Refine [n : Integer] (<= n (vector-length v)))])
        Any)
    (make-DepFun (list (-vec Univ)
                       (-refine/fresh n -Int
                                      (-leq (-lexp (-id-path n))
                                            (-lexp (-vec-len-of (-id-path (cons 1 0)))))))
                 -tt
                 (-values Univ))]
   [(-> ([v : (Vectorof Any)]
         [i : (v) (Refine [n : Integer] (<= n (vector-length v)))])
        Any)
    (dep-> ([x : (-vec Univ)]
            [y : (-refine/fresh n -Int
                                (-leq (-lexp (-id-path n))
                                      (-lexp (-vec-len-of (-id-path x)))))])
           Univ)]
   [(-> ([i : (v) (Refine [n : Integer] (<= n (vector-length v)))]
         [v : (Vectorof Any)])
        Any)
    (make-DepFun (list (-refine/fresh n -Int
                                      (-leq (-lexp (-id-path n))
                                            (-lexp (-vec-len-of (-id-path (cons 1 1))))))
                       (-vec Univ))
                 -tt
                 (-values Univ))]
   [(-> ([x : Integer]
         [y : (z) (Refine [n : Integer] (<= n z))]
         [z : (x) (Refine [n : Integer] (<= n x))])
        Any)
    (dep-> ([x : -Int]
            [y : (-refine/fresh n -Int
                                    (-leq (-lexp n)
                                          (-lexp z)))]
            [z : (-refine/fresh n -Int
                                    (-leq (-lexp n)
                                          (-lexp x)))])
           Univ)]
   [(-> ([x : Integer]
         [y : (z) (Refine [n : Integer] (<= n z))]
         [z : (x) (Refine [n : Integer] (<= n x))])
        Any)
    (make-DepFun
     (list -Int
           (-refine/fresh n -Int
                          (-leq (-lexp n)
                                (-lexp (-id-path (cons 1 2)))))
           (-refine/fresh n -Int
                          (-leq (-lexp n)
                                (-lexp (-id-path (cons 1 0))))))
     -tt
     (-values Univ))]
   [(-> ([w : (y) (Refine [n : Integer] (<= n y))]
         [x : Integer]
         [y : (z x) (Refine [n : Integer] (<= n (+ x z)))]
         [z : (x) (Refine [n : Integer] (<= n x))])
        (Refine [n : Integer] (<= n (+ w x y z))))
    (make-DepFun
     (list (-refine/fresh n -Int (-leq (-lexp n) (-lexp (-id-path (cons 1 2)))))
           -Int
           (-refine/fresh n -Int (-leq (-lexp n)
                                       (-lexp (-id-path (cons 1 1))
                                              (-id-path (cons 1 3)))))
           (-refine/fresh n -Int (-leq (-lexp n)
                                       (-lexp (-id-path (cons 1 1))))))
     -tt
     (-values
      (-refine/fresh n -Int (-leq (-lexp n)
                                  (-lexp (-id-path (cons 1 0))
                                         (-id-path (cons 1 1))
                                         (-id-path (cons 1 2))
                                         (-id-path (cons 1 3)))))))]
   [(-> ([w : (y) (Refine [n : Integer] (<= n y))]
         [x : Integer]
         [y : (z x) (Refine [n : Integer] (<= n (+ x z)))]
         [z : (x) (Refine [n : Integer] (<= n x))])
        (Refine [n : Integer] (<= n (+ w x y z))))
    (dep-> ([w : (-refine/fresh n -Int (-leq (-lexp n) (-lexp y)))]
            [x : -Int]
            [y : (-refine/fresh n -Int (-leq (-lexp n)
                                             (-lexp x z)))]
            [z : (-refine/fresh n -Int (-leq (-lexp n)
                                             (-lexp x)))])
           (-refine/fresh n -Int (-leq (-lexp n)
                                       (-lexp w x y z))))]
   ;; #:pre condition
   [(-> ([w : Integer]
         [x : Integer]
         [y : Integer]
         [z : Integer])
        #:pre (w x y z)
        (and (<= w y)
             (<= y (+ x z))
             (<= z x))
        (Refine [n : Integer] (<= n (+ w x y z))))
    (dep-> ([w : -Int]
            [x : -Int]
            [y : -Int]
            [z : -Int])
           #:pre (-and (-leq (-lexp w) (-lexp y))
                       (-leq (-lexp y) (-lexp x z))
                       (-leq (-lexp z) (-lexp x)))
           (-refine/fresh n -Int (-leq (-lexp n)
                                       (-lexp w x y z))))]

   ;; shadowing
   [(-> ([x : Integer]
         [y : (x) (Refine [n : Integer] (<= n x))]
         [z : (x y) (Refine [y : Integer] (<= y x))])
        Any)
    (dep-> ([x : -Int]
            [y : (-refine/fresh n -Int
                                (-leq (-lexp n)
                                      (-lexp x)))]
            [z : (-refine/fresh n -Int
                                (-leq (-lexp n)
                                      (-lexp x)))])
           Univ)]
   ;; shadowing (and thus not really dependent in this case)
   [(-> ([x : Any]
         [z : (x) (Refine [x : Integer] (<= x 42))])
        Any)
    (t:->
     Univ
     (-refine/fresh n -Int
                    (-leq (-lexp n)
                          (-lexp 42)))
     Univ)]
   ;; shadowing
   [(-> ([x : Any]
         [y : Any])
        (Refine [x : Integer] (<= x 42)))
    (t:-> Univ Univ (-refine/fresh res -Int (-leq (-lexp res) (-lexp 42))))]



   ;; duplicate ids
   [FAIL (-> ([x : Integer]
              [x : Integer])
             Integer)]
   ;; duplicate dependencies
   [FAIL (-> ([x : (y y) Integer]
              [y : Integer])
             Integer)]
   [FAIL (-> ([x : Integer]
              [y : Integer])
             #:pre (x x y)
             (<= x y)
             Integer)]
   ;; listing self in dependency list
   [FAIL (-> ([x : (x y) Integer]
              [y : Integer])
             Integer)]
   ;; missing colon
   [FAIL (-> ([x : Integer]
              [y Integer])
             Integer)]
   ;; unbound ids
   [FAIL (-> ([x : (Refine [n : Integer] (= n this-is-an-unbound-identifier))]
              [y : Integer])
             Integer)]
   [FAIL (-> ([x : (this-is-an-unbound-identifier)
                 (Refine [n : Integer] (= n this-is-an-unbound-identifier))]
              [y : Integer])
             Integer)]
   [FAIL (-> ([x : (Refine [n : Integer] (= n fun-arg))]
              [fun-arg : Integer])
             Integer)]
   [FAIL (-> ([x : (z) (Refine [n : Integer] (= n fun-arg))]
              [fun-arg : Integer]
              [z : Integer])
             Integer)]
   [FAIL (-> ([x : Integer]
              [y : Integer])
             #:pre (x y)
             (and (<= x y)
                  (<= x this-is-an-unbound-identifier))
             Integer)]
   [FAIL (-> ([x : Integer]
              [y : Integer])
             #:pre (x y this-is-an-unbound-identifier)
             (and (<= x y)
                  (<= x this-is-an-unbound-identifier))
             Integer)]
   [FAIL (-> ([x : Integer]
              [y : Integer])
             (Refine [n : Integer] (= n this-is-an-unbound-identifier)))]
   ;; cyclic dependencies
   [FAIL (-> ([x : (y) (Refine [n : Integer] (= n y))]
              [y : (x) (Refine [n : Integer] (= n x))])
             Integer)]
   [FAIL (-> ([x : (y) (Refine [n : Integer] (= n y))]
              [y : (z) (Refine [n : Integer] (= n z))]
              [y : (x) (Refine [n : Integer] (= n x))])
             Integer)]
   ;; shadowing w/ bad types
   [FAIL (-> ([x : Integer]
              [z : (x) (Refine [x : Univ] (<= x 42))])
             Any)]
   [FAIL (-> ([x : Integer]
              [y : Integer])
             (Refine [x : Univ] (<= x 42)))]

   ))

;; FIXME - add tests for parse-values-type, parse-tc-results
