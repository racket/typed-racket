#lang racket/base
(require "test-utils.rkt"
         "evaluator.rkt"
         (for-syntax
           racket/base
           racket/dict
           racket/set
           syntax/parse
           (base-env base-structs)
           (env tvar-env type-alias-env mvar-env)
           (utils tc-utils)
           (private parse-type)
           (rep type-rep values-rep)

           (submod typed-racket/base-env/base-types initialize)
           (rename-in (types union abbrev numeric-tower resolve)
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
                #`(values #,expected #,actual #,(type-equal? actual expected)))))
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
   [(->* (String Symbol) Void) (t:-> -String -Symbol -Void)]
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
   [Struct-TypeTop (make-StructTypeTop)]

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
   [UnitTop (make-UnitTop)]
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
   [(∩ (-> Number Number) (-> String String))
    (-unsafe-intersect (t:-> -String -String)
                       (t:-> -Number -Number))]
   ))

;; FIXME - add tests for parse-values-type, parse-tc-results
