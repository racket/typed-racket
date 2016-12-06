#lang racket/base

(require "test-utils.rkt"
         (types subtype numeric-tower utils abbrev)
         (rep type-rep values-rep)
         (env init-envs type-env-structs)
         rackunit
         (for-syntax racket/base))

(provide tests)
(gen-test-main)

(define-for-syntax (single-subtype-test stx)
  (syntax-case stx (FAIL)
    [(FAIL t s) (syntax/loc stx (test-check (format "FAIL ~a" '(t s)) (lambda (a b) (not (subtype a b))) t s))]
    [(t s) (syntax/loc stx (test-check (format "~a" '(t s)) subtype t s))]))

(define-syntax (subtyping-tests stx)
  (syntax-case stx ()
    [(_ str cl ...)
     (with-syntax ([(new-cl ...) (map single-subtype-test (syntax->list #'(cl ...)))])
       (syntax/loc stx
         (begin (test-suite (format "Tests for subtyping (~a)" str)
                            new-cl ...))))]))

(define x1 #'x)
(define x2 #'x)
(define A (make-F (gensym 'A)))
(define B (make-F (gensym 'B)))
(define AorB (Un A B))
(define t1a (-mu T (-lst (Un (-v a) T))))
(define t1b (unfold t1a))
(define t2 -Number)

(define-for-syntax (covariant-test stx)
  (syntax-case stx ()
    [(mk []) (syntax/loc stx
               (subtyping-tests
                (format "covariant tests for ~a" mk)
                [(mk A) (mk A)]
                [(mk t1a) (mk t1b)]
                [(mk A) (mk AorB)]
                [FAIL (mk AorB) (mk A)]
                [FAIL (mk Univ) A]
                [FAIL A (mk Univ)]))]
    [(mk [] [])
     (syntax/loc stx
       (subtyping-tests
        (format "covariant tests for ~a" mk)
        [(mk A B) (mk A B)]
        [(mk A B) (mk AorB B)]
        [(mk A B) (mk A AorB)]
        [(mk A B) (mk AorB AorB)]
        [(mk t1a t1a) (mk t1b t1b)]
        [FAIL (mk AorB B) (mk A B)]
        [FAIL (mk A AorB) (mk A B)]
        [FAIL (mk AorB AorB) (mk A B)]
        [FAIL (mk Univ Univ) A]
        [FAIL A (mk Univ Univ)]))]))

(define-syntax (covariant-tests stx)
  (syntax-case stx ()
    [(_ cl ...)
     (with-syntax ([(co-tests ...) (map covariant-test (syntax->list #'(cl ...)))])
       (syntax/loc stx
         (begin co-tests ...)))]))

(define-for-syntax (invariant-test stx)
  (syntax-case stx ()
    [(mk () #:top top)
     (syntax/loc stx
       (subtyping-tests
        (format "invariant tests for ~a" mk)
        [(mk A) (mk A)]
        [(mk t1a) (mk t1b)]
        [(mk A) top]
        [FAIL top (mk A)]
        [FAIL (mk A) (mk AorB)]
        [FAIL (mk AorB) (mk A)]
        [FAIL (mk Univ) A]
        [FAIL A (mk Univ)]))]
    [(mk () () #:top top)
     (syntax/loc stx
       (subtyping-tests
        (format "invariant tests for ~a" mk)
        [(mk A B) (mk A B)]
        [(mk t1a t1a) (mk t1b t1b)]
        [(mk A B) top]
        [FAIL top (mk A B)]
        [FAIL (mk A B) (mk AorB B)]
        [FAIL (mk A B) (mk A AorB)]
        [FAIL (mk A B) (mk AorB AorB)]
        [FAIL (mk AorB B) (mk A B)]
        [FAIL (mk A AorB) (mk A B)]
        [FAIL (mk AorB AorB) (mk A B)]
        [FAIL (mk Univ Univ) A]
        [FAIL A (mk Univ Univ)]))]))

(define-syntax (invariant-tests stx)
  (syntax-case stx ()
    [(_ cl ...)
     (with-syntax ([(inv-tests ...) (map invariant-test (syntax->list #'(cl ...)))])
       (syntax/loc stx
         (begin inv-tests ...)))]))


(define simple-tests
  (subtyping-tests
   "Simple Subtyping Tests"
   ;; trivial (⊤) examples
   [Univ Univ]
   [-Bottom Univ]
   [-Number Univ]
   [-Boolean Univ]
   [-Symbol Univ]
   [-Void Univ]
   [FAIL Univ -Symbol]
   [FAIL Univ -Number]
   ;; Reflexivity
   [(-val 6) (-val 6)]
   [-Symbol -Symbol]
   ;; Error
   [Err -Number]
   [-Number Err]
   ;; B
   [(make-B 0) (make-B 0)]
   [FAIL (make-B 0) (make-B 1)]
   ;; F
   [(-v a) (-v a)]
   [FAIL (-v a) (-v b)]
   ;; Value
   [(-val 'a) (-val 'a)]
   [(-val 'a) -Symbol]
   [FAIL -Symbol (-val 'a)]
   [FAIL (-val 'a) (-val 'b)]
   ;; Base
   [-String -String]
   [FAIL -Symbol -String]
   ;; Opaque
   [(make-Opaque x1) (make-Opaque x2)]
   [FAIL (make-Opaque #'a) (make-Opaque #'b)]
   ;; Refinement
   [(make-Refinement A x1) (make-Refinement A x2)]
   ;[(make-Refinement t1a x1) (make-Refinement t1b x1)]
   ;[(make-Refinement t1a x1) t1b]
   [FAIL (make-Refinement t2 x1) t1b]
   [FAIL (make-Refinement A #'a) (make-Refinement A #'b)]
   [FAIL (make-Refinement A x1) (make-Refinement B x1)]
   ;; Distinction
   [(-Distinction 'a 'b A) A]
   [(-Distinction 'a 'b A) AorB]
   [FAIL (-Distinction 'a 'b AorB) A]
   ))

(define structural-tests
  (test-suite
   "Structural Subtyping tests"
   (covariant-tests
    [make-Pair () ()]
    [make-Promise ()]
    [make-Ephemeron ()]
    [make-CustodianBox ()]
    [make-Set ()]
    [make-Evt ()]
    [make-Syntax ()]
    [make-Future ()])
   (invariant-tests
    [make-MPair () () #:top -MPairTop]
    [make-Vector () #:top -VectorTop]
    [make-Box () #:top -BoxTop]
    [make-Channel () #:top -ChannelTop]
    [make-Async-Channel () #:top -Async-ChannelTop]
    [make-ThreadCell () #:top -ThreadCellTop]
    [make-Weak-Box () #:top -Weak-BoxTop]
    [make-Hashtable () () #:top -HashtableTop]
    [make-Prompt-Tagof () () #:top -Prompt-TagTop]
    [make-Continuation-Mark-Keyof () #:top -Continuation-Mark-KeyTop])
   (subtyping-tests
    "Param"
    [(make-Param A B) (make-Param A B)]
    [(make-Param A B) (make-Param A AorB)]
    [(make-Param AorB B) (make-Param A B)]
    [FAIL (make-Param A B) (make-Param AorB B)]
    [FAIL (make-Param A AorB) (make-Param A B)])
   (subtyping-tests
    "Evt special cases"
    ;; evts
    [(-evt t1a) (-evt t1b)]
    [FAIL (-evt -Byte) (-evt -String)]
    [-Semaphore (-evt -Semaphore)]
    [FAIL -Semaphore (-evt -Int)]
    [-Output-Port (-evt -Output-Port)]
    [FAIL -Output-Port (-evt -Int)]
    [-Input-Port (-evt -Input-Port)]
    [FAIL -Input-Port (-evt -Int)]
    [-TCP-Listener (-evt -TCP-Listener)]
    [FAIL -TCP-Listener (-evt -Int)]
    [-Thread (-evt -Thread)]
    [FAIL -Thread (-evt -Int)]
    [-Subprocess (-evt -Subprocess)]
    [FAIL -Subprocess (-evt -Int)]
    [-Will-Executor (-evt -Will-Executor)]
    [FAIL -Will-Executor (-evt -Int)]
    [(make-CustodianBox -String) (-evt (make-CustodianBox -String))]
    [FAIL (make-CustodianBox -String) (-evt -String)]
    [(-channel -String) (-evt -String)]
    [FAIL (-channel -String) (-evt -Int)]
    [-Log-Receiver (-evt (make-HeterogeneousVector
                          (list -Symbol -String Univ
                                (Un (-val #f) -Symbol))))]
    [FAIL -Log-Receiver (-evt -Int)])
   (subtyping-tests
    "Sequence special cases"
    [(-set -Number) (make-Sequence (list -Number))]
    [-FlVector (make-Sequence (list -Flonum))]
    [-FlVector (make-Sequence (list -Number))]
    [-FxVector (make-Sequence (list -Fixnum))]
    [-FxVector (make-Sequence (list -Number))]
    [(-val 5) (-seq -Nat)]
    [(-val 5) (-seq -Byte)]
    [-Index (-seq -Index)]
    [-NonNegFixnum (-seq -NonNegFixnum)]
    [-Index (-seq -Nat)]
    [FAIL (-val -5) (-seq -Nat)]
    [FAIL -Fixnum (-seq -Fixnum)]
    [FAIL -NonNegFixnum (-seq -Index)]
    [FAIL (-val 5.0) (-seq -Nat)]
    [(-pair -String (-lst -String)) (-seq -String)]
    [FAIL (-pair -String (-lst -Symbol)) (-seq -String)]
    [FAIL (-pair -String (-vec -String)) (-seq -String)]
    [(-mpair -String -Null) (-seq -String)]
    [(-mlst -String) (-seq -String)]
    [(-mpair -String (-mlst -String)) (-seq -String)]
    [FAIL (-mpair -String (-mlst -Symbol)) (-seq -String)]
    [FAIL (-mpair -String (-vec -String)) (-seq -String)]
    [(-mpair -String (-mlst (-val "hello"))) (-seq -String)])
   (subtyping-tests
    "Other Structural Cases"
    [(-Param -String -Symbol) (cl->* (-> -Symbol) (-> -String -Void))]
    [(-lst* -Number -Number (-val 'foo)) (-lst (Un -Number -Symbol))])))

(define other-tests
  (subtyping-tests
   "Other Subtyping Tests"
   ;; recursive types and unions
   [(Un (-pair Univ (-lst Univ)) -Null) (-lst Univ)]
   [(-lst* -Number -Number (-val 'foo)) (-lst Univ)]
   [(Un (-val #f) (Un (-val 6) (-val 7))) (-mu x (Un -Number (Un -Boolean -Symbol)))]
   [(Un -Number (-val #f) (-mu x (Un -Number -Symbol (make-Listof x))))
    (-mu x (Un -Number -Symbol -Boolean (make-Listof x)))]
   ;; sexps vs list*s of nums
   [(-mu x (Un -Number -Symbol (make-Listof x))) (-mu x (Un -Number -Symbol -Boolean (make-Listof x)))]
   [(-mu x (Un -Number (make-Listof x))) (-mu x (Un -Number -Symbol (make-Listof x)))]
   [(-mu x (Un -Number (make-Listof x))) (-mu y (Un -Number -Symbol (make-Listof y)))]
   ;; a hard one
   [(-mu x (Un -Number (-lst* x -Symbol x))) -Sexp]
   [t1a (unfold t1a)]
   [(unfold t1a) t1a]
   ;; simple list types
   [(make-Listof -Number) (make-Listof Univ)]
   [(make-Listof -Number) (make-Listof -Number)]
   [FAIL (make-Listof -Number) (make-Listof -Symbol)]
   [(-mu x (make-Listof x)) (-mu x* (make-Listof x*))]
   [(-pair -Number -Number) (-pair Univ -Number)]
   [(-pair -Number -Number) (-pair -Number -Number)]
   ;; from page 7 (my favorite page! But seriously, page 7 of... what???)
   [(-mu t (-> t t)) (-mu s (-> s s))]
   [(-mu s (-> -Number s)) (-mu t (-> -Number (-> -Number t)))]
    
   ;; not subtypes
   [FAIL (-val 'hello) -Number]
   [FAIL (-val #f) -Symbol]
   [FAIL (Univ Univ -Number -Number . -> . -Number) (Univ Univ Univ . -> . -Number)]
   [FAIL (-Number . -> . -Number) (-> Univ Univ)]
   [FAIL (Un -Number -Symbol) -Number]
   [FAIL -Number (Un (-val 6) (-val 11))]
   [FAIL -Symbol (-val 'Sym)]
   [FAIL (Un -Symbol -Number) (-poly (a) -Number)]
   ;; bugs found
    
   [(-poly (a) (make-Listof (-v a))) (make-Listof (-mu x (Un (make-Listof x) -Number)))]
   [FAIL (make-Listof (-mu x (Un (make-Listof x) -Number))) (-poly (a) (make-Listof a))]

   ;; HeterogeneousVector
   [(make-HeterogeneousVector (list t1a)) (-vec t1b)]
   [(make-HeterogeneousVector (list t1a t1a)) (-vec t1b)]
   [FAIL (-vec t1b) (make-HeterogeneousVector (list t1a t1a))]
   [FAIL (make-HeterogeneousVector (list t2)) (-vec t1b)]
   [FAIL (make-HeterogeneousVector (list t1a t2)) (-vec t1b)]
   [(make-HeterogeneousVector (list t1a t1b)) (make-HeterogeneousVector (list t1b t1a))]
   [(make-HeterogeneousVector (list t1a t1b)) (make-HeterogeneousVector (list t1b t1a))]
   [FAIL (make-HeterogeneousVector (list t1a)) (make-HeterogeneousVector (list t1b t1a))]
   [FAIL (make-HeterogeneousVector (list t1a t2)) (make-HeterogeneousVector (list t1b t1a))]
   [FAIL (make-HeterogeneousVector (list t2 t1a)) (make-HeterogeneousVector (list t1b t1a))]
   ))

(define set-theoretic-type-tests
  (subtyping-tests
   "Set-theoretic Subtyping"
   ;; Unions
   [(-val 0.0f0) -SingleFlonum]
   [(-val -0.0f0) -SingleFlonum]
   [(-val 1.0f0) -SingleFlonum]
   [(-val -34.2f0) -NegSingleFlonum]
   [(-val 6) -Number]
   [(Un (-val 'foo) (-val 6)) (Un (-val 'foo) (-val 6))]
   [(Un -Number) -Number]
   [(Un -Number -Number) -Number]
   [(Un -Number -Symbol) (Un -Symbol -Number)]
   [(Un (-val 6) (-val 7)) -Number]
   [(Un (-val #f) (Un (-val 6) (-val 7))) (Un -Number (Un -Boolean -Symbol))]
   ;; intersections
   [(-unsafe-intersect -Number) -Number]
   [(-unsafe-intersect -Number -Symbol) -Number]
   [(-unsafe-intersect -Boolean -Number) -Number]
   [(-unsafe-intersect -Number -Number) -Number]
   [FAIL -Number (-unsafe-intersect -Boolean -Number)]
   [(-unsafe-intersect -Boolean -Number) (-unsafe-intersect -Boolean -Number)]
   [(-unsafe-intersect -Sexp
                       (Un -Null (-pair -Sexp (-unsafe-intersect (make-Listof Univ) -Sexp))))
    (make-Listof Univ)]
   [(-unsafe-intersect (-v A) (-v B))
    (Un -String (-unsafe-intersect (-v A) (-v B)))]
   ))


(define poly-tests
  (subtyping-tests
   "Polymorphic Subtyping"
   [(-poly (t) (-> t t)) (-poly (s) (-> s s))]
   [FAIL (make-Listof -Number) (-poly (t) (make-Listof t))]
   [(-poly (a) (make-Listof (-v a))) (make-Listof -Number)]     ;;
   [(-poly (a) -Number) -Number]
   [FAIL (-poly (a) (-poly (b) (-pair a b))) (-poly (a) (-poly (b) (-pair b a)))]

   ;; The following currently are not subtypes, because they are not replaceable
   ;; in an instantiation context. It may be sound for them to be subtypes but
   ;; the implications of that change are unknown.
   [FAIL (-poly (x) (-lst x)) (-poly (x y) (-lst x))]
   [FAIL (-poly (y z) (-lst y)) (-poly (z y) (-lst y))]
   [FAIL (-poly (y) (-poly (z) (-pair y z))) (-poly (y z) (-pair y z))]
   [FAIL (-poly (y z) (-pair y z)) (-poly (y) (-poly (z) (-pair y z)))]))

(define function-tests
  (subtyping-tests
   "Function Subtyping"
   ;; simple function types
   [((Un -Symbol -Number) . -> . -Number) (-> -Number -Number)]
   [(-poly (t) (-> -Number t)) (-mu t (-> -Number t))]
   ((Univ . -> . -Number) (-Number . -> . Univ))
   [(Univ Univ Univ . -> . -Number) (Univ Univ -Number . -> . -Number)]
   ;; case-lambda
   [(cl-> [(-Number) -Number] [(-Boolean) -Boolean]) (-Number . -> . -Number)]
   ;; special case for unused variables
   [-Number (-poly (a) -Number)]
   [FAIL (cl-> [(-Number) -Boolean] [(-Boolean) -Number]) (-Number . -> . -Number)]
   ;; varargs
   [(->* (list -Number) Univ -Boolean) (->* (list -Number) -Number -Boolean)]
   [(->* (list Univ) -Number -Boolean) (->* (list -Number) -Number -Boolean)]
   [(->* (list -Number) -Number -Boolean) (->* (list -Number) -Number -Boolean)]
   [(->* (list -Number) -Number -Boolean) (->* (list -Number) -Number Univ)]
   [(->* (list -Number) -Number -Number) (->* (list -Number -Number) -Number)]
   [(->* (list -Number) -Number -Number) (->* (list -Number -Number -Number) -Number)]
   [(->* (list -Number -Number) -Boolean -Number) (->* (list -Number -Number) -Number)]
   [FAIL (->* (list -Number) -Number -Boolean) (->* (list -Number -Number -Number) -Number)]
   [(->* (list -Number -Number) -Boolean -Number) (->* (list -Number -Number -Boolean -Boolean) -Number)]

   [(-poly (a) (cl-> [() a]
                     [(-Number) a]))
    (cl-> [() (-pair -Number (-v b))]
          [(-Number) (-pair -Number (-v b))])]
   ;; polymorphic function types should be subtypes of the function top
   [(-poly (a) (a . -> . a)) top-func]
   [FAIL (-> Univ) (null Univ . ->* . Univ)]

   [(-poly (b) ((Un (make-Base 'foo #'dummy values #f)
                    (-struct #'bar #f
                             (list (make-fld -Number #'values #f) (make-fld b #'values #f))))
                . -> . (-lst b)))
    ((Un (make-Base 'foo #'dummy values #f) (-struct #'bar #f (list (make-fld -Number #'values #f) (make-fld (-pair -Number (-v a)) #'values #f))))
     . -> . (-lst (-pair -Number (-v a))))]
   [(-poly (b) ((-struct #'bar #f (list (make-fld -Number #'values #f) (make-fld b #'values #f))) . -> . (-lst b)))
    ((-struct #'bar #f (list (make-fld -Number #'values #f) (make-fld (-pair -Number (-v a)) #'values #f)))
     . -> .
     (-lst (-pair -Number (-v a))))]

   [(-poly (a) (a . -> . (make-Listof a))) ((-v b) . -> . (make-Listof (-v b)))]
   [(-poly (a) (a . -> . (make-Listof a))) ((-pair -Number (-v b)) . -> . (make-Listof (-pair -Number (-v b))))]
   [FAIL (-poly (a b) (-> a a)) (-poly (a b) (-> a b))]
    
   [(cl->* (-Number . -> . -String) (-Boolean . -> . -String)) ((Un -Boolean -Number) . -> . -String)]
   [(-> Univ -Boolean : (-PS (-is-type 0 -Symbol) (-not-type 0 -Symbol)))
    (-> Univ -Boolean : -tt-propset)]
   [(-> Univ -Boolean : -ff-propset)
    (-> Univ -Boolean : (-PS (-is-type 0 -Symbol) (-not-type 0 -Symbol)))]
   [(-> Univ -Boolean : (-PS (-is-type 0 -Symbol) (-not-type 0 -Symbol)))
    (-> (Un -Symbol -String) -Boolean : (-PS (-is-type 0 -Symbol) (-not-type 0 -Symbol)))]
   [FAIL
    (-> Univ -Boolean : (-PS (-is-type 0 -Symbol) (-not-type 0 -Symbol)))
    (-> Univ -Boolean : (-PS (-is-type 0 -String) (-not-type 0 -String)))]

   ;; subtyping for types inside propositions
   [(-> Univ -Boolean : (-PS (-is-type 0 -Symbol) (-not-type 0 -Symbol)))
    (-> Univ -Boolean : (-PS (-is-type 0 (-opt -Symbol)) (-not-type 0 -Symbol)))]
   [(-> Univ -Boolean : (-PS (-is-type 0 -Symbol) (-not-type 0 (-opt -Symbol))))
    (-> Univ -Boolean : (-PS (-is-type 0 -Symbol) (-not-type 0 -Symbol)))]
   [FAIL
    (-> Univ -Boolean : (-PS (-is-type 0 (-opt -Symbol)) (-not-type 0 (-opt -Symbol))))
    (-> Univ -Boolean : (-PS (-is-type 0 -Symbol) (-not-type 0 -Symbol)))]

   [FAIL (make-ListDots (-box (make-F 'a)) 'a) (-lst (-box Univ))]
   [(make-ListDots (-> -Symbol (make-F 'a)) 'a) (-lst (-> -Symbol Univ))]

   [(-> Univ -Bottom) (-> Univ (-values (list -String -Symbol)))]
   [(-> Univ -Bottom) (-> Univ (-values-dots null -String 'x))]

   [FAIL (make-pred-ty -String) (-> Univ (-AnyValues (-is-type 0 -String)))]

   ;; keyword function types
   [(->key #:x -Symbol #f Univ) (->key Univ)]
   [FAIL (->key #:x -Symbol #t Univ) (->key Univ)]
   [FAIL (->key Univ) (->key #:x -Symbol #t Univ)]
   [(->key #:x -Symbol #f #:y -Symbol #f Univ) (->key Univ)]
   [FAIL (->key #:x -Symbol #f #:y -Symbol #t Univ) (->key Univ)]
   [(->key #:x -Symbol #f #:y -Symbol #f Univ) (->key #:x -Symbol #f Univ)]
   [(->key #:x -Symbol #f #:y -Symbol #f Univ) (->key #:x -Symbol #t Univ)]
   [FAIL (->key #:x -Symbol #f Univ) (->key #:x -Symbol #f #:y -Symbol #f Univ)]
   [(->key #:x -Symbol #f #:y -Symbol #f Univ)
    (->key #:x -Symbol #t #:y -Symbol #t Univ)]
   [FAIL
    (->key #:x -Symbol #t #:y -Symbol #f Univ)
    (->key #:x -Symbol #f #:y -Symbol #t Univ)]
   [(->key #:x (-opt -String) #f #:y -Symbol #f Univ)
    (->key #:x -String #t Univ)]
   [FAIL
    (->key #:x -String #f #:y -Symbol #f Univ)
    (->key #:x (-opt -String) #t Univ)]
   [(->key -String #:x -Symbol #f #:y -Symbol #f Univ)
    (->key -String #:x -Symbol #t Univ)]
   [FAIL
    (->key -String #:x -Symbol #f #:y -Symbol #f Univ)
    (->key -Void #:x -Symbol #t Univ)]
   [(->optkey -String [-String] #:x -Symbol #f #:y -Symbol #f Univ)
    (->key -String #:x -Symbol #t Univ)]
   [(->optkey -String [-String] #:x -Symbol #f #:y -Symbol #f Univ)
    (->optkey -String [-String] #:x -Symbol #t Univ)]
   [FAIL
    (->optkey -String [-String] #:x -Symbol #f #:y -Symbol #f Univ)
    (->optkey -String [-Void] #:x -Symbol #t Univ)]
   [FAIL
    (->key -String #:x -Symbol #f #:y -Symbol #f Univ)
    (->optkey -String [-Void] #:x -Symbol #t Univ)]
   
   ;; Proposition subtyping
   [(make-pred-ty (list -Real) -Boolean (Un (-val 0.0) (-val 0)))
    (make-pred-ty (list -Int) -Boolean (-val 0))]

   [(-polydots (a) (->... (list Univ) (a a) (make-ValuesDots null a 'a)))
    (-polydots (a) (->... (list -String) (a a) (make-ValuesDots null a 'a)))]

   [(-polydots (a) (->... null (Univ a) (make-ValuesDots (list (-result a)) a 'a)))
    (-polydots (a) (->... null (-String a) (make-ValuesDots (list (-result a)) a 'a)))]

   [(-polydots (a) (->... null (a a) (make-ValuesDots (list (-result -String)) -String 'a)))
    (-polydots (a) (->... null (a a) (make-ValuesDots (list (-result Univ)) Univ 'a)))]

   [(-polydots (a) (->... null (Univ a) (-values (list Univ))))
    (->* null Univ Univ)]

   ;; ListDots
   [(-polydots (a) (->... null (a a) (make-ListDots a 'a)))
    (-> -String -Symbol (-Tuple (list -String -Symbol)))]
   [(-> -String -Symbol (-Tuple (list -String -Symbol)))
    (-polydots (a) (-> -String -Symbol (-lst (Un -String -Symbol))))]

   [(-polydots (a) (->... null (a a) (make-ListDots a 'a)))
    (-poly (a b) (-> a b (-Tuple (list a b))))]

   [(-polydots (b a) (-> (->... (list b) (a a) (make-ValuesDots (list (-result b)) a 'a)) Univ))
    (-polydots (a) (-> (->... (list) (a a) (make-ValuesDots null a 'a)) Univ))]

   [(-polydots (a) (->... (list) (a a) (make-ListDots a 'a)))
    (-polydots (b a) (->... (list b) (a a) (-pair b (make-ListDots a 'a))))]

   [FAIL
    (-polydots (c a b) (->... (list (->... (list a) (b b) c) (-vec a)) ((-vec b) b) (-vec c)))
    (->* (list (->* (list) -Symbol -Symbol)) (-vec -Symbol) (-vec -Symbol))]))

(define struct-tests
  (subtyping-tests
   "Struct Subtyping"
   [(-struct x1 #f null) (-struct x2 #f null)]
   [(-struct #'a #f (list (make-fld -String #'values #f))) (-struct #'a #f (list (make-fld -String #'values #f)))]
   [(-struct #'a #f (list (make-fld -String #'values #f))) (-struct #'a #f (list (make-fld Univ #'values #f)))]
   ;; prefab structs
   [(-prefab 'foo -String) (-prefab 'foo -String)]
   [(-prefab 'foo -String) (-prefab 'foo (-opt -String))]
   [(-prefab '(bar foo 1) -String -Symbol) (-prefab 'foo -String)]
   [(-prefab '(bar foo 1) -String -Symbol) (-prefab 'foo (-opt -String))]
   [FAIL
    (-prefab '(foo #(0)) -String) (-prefab '(foo #(0)) (-opt -String))]
   [(-prefab '(foo 1 #(0)) -String -Symbol)
    (-prefab '(foo #(0)) -String)]
   [(-prefab '(bar foo 1 #(0)) -String -Symbol)
    (-prefab '(foo #(0)) -String)]
   [FAIL
    (-prefab '(foo #()) -String) (-prefab '(foo #(0)) (-opt -String))]
   ;; TODO StructType ?
   ))

(define oo-tests
  (subtyping-tests
   "Object Oriented Subtyping"
   [(-class #:method ((m (-> -Nat))) #:augment ((m (-> -Nat))))
    (-class #:method ((m (-> -Nat))) #:augment ((m (-> -Nat))))]
   [(-object #:method ((m (-> -Nat))) #:augment ((m (-> -Nat))))
    (-object #:method ((m (-> -Nat))) #:augment ((m (-> -Nat))))]
   [(-object #:method ((m (-> -Nat)) (n (-> -Nat))))
    (-object #:method ((m (-> -Nat))))]
   [(-object #:method ((f (-> -Nat))) #:augment ((m (-> -Nat)) (n (-> -Nat))))
    (-object #:augment ((m (-> -Nat))))]
   [(-object #:field ((a -Nat)) #:method ((m (-> -Nat)) (n (-> -Nat))))
    (-object #:method ((m (-> -Nat))))]
   [(-object #:field ((x -Symbol)))
    (-object #:field ((x -Symbol)))]
   [(-object #:field ((x -Symbol)))
    (-object #:field ((x (Un -Symbol (-val #f)))))]
   [FAIL
    (-object #:field ((a -Symbol)))
    (-object #:field ((x -Symbol)))]
   [FAIL
    (-object #:field ((a -Symbol)))
    (-object #:field ((x -String)))]
   [FAIL
    (-object #:field ((x -Symbol)))
    (-object #:field ((x -String)))]
   [FAIL
    (-object #:method ((m (-> -String)) (n (-> -String))))
    (-object #:method ((x (-> -String))))]
   [(-object #:method ((m (-> -String)) (n (-> -String))))
    (-object #:method ((m (-> -String))))]
   [FAIL
    (-object #:method ())
    (-object #:method ((m (-> -String))))]
   [FAIL
    (-object #:method ((m (-> -Nat)) (n (-> -Nat))))
    (-object #:method ((l (-> -Nat)) (m (-> -Nat))))]
   [(-object #:method ((m (-> -Nat)) (n (-> -Nat))))
    (-object #:method ((n (-> -Nat)) (m (-> -Nat))))]
   [FAIL
    (-class #:method ((m (-> -Nat))))
    (-class #:method ((m (-> -Nat))) #:augment ((m (-> -Nat))))]
   [FAIL
    (-class #:method ((m (-> -Nat))) #:augment ((m (-> -Nat))))
    (-class #:method ((m (-> -Nat))))]))

(define-for-syntax (single-subval-test stx)
  (syntax-case stx (FAIL)
    [(FAIL t s) (syntax/loc stx (test-check (format "FAIL ~a" '(t s)) (lambda (a b) (not (subval a b))) t s))]
    [(t s) (syntax/loc stx (test-check (format "~a" '(t s)) subval t s))]))

(define-syntax (subval-tests stx)
  (syntax-case stx ()
    [(_ str cl ...)
     (with-syntax ([(new-cl ...) (map single-subval-test (syntax->list #'(cl ...)))])
       (syntax/loc stx
         (begin (test-suite (format "Tests for subval (~a)" str)
                            new-cl ...))))]))

(define values-tests
  (subval-tests
   "SomeValues"
   [(-values (list -Number))
    (-values (list Univ))]
   [FAIL (make-ValuesDots (list) -Symbol 'a)
         (make-ValuesDots (list (-result -String)) -String 'a)]
   [(-values (list -Bottom))
    (-values (list -String -Symbol))]
   ))



(define tests
  (test-suite
   "All Subtype Tests"
   simple-tests
   structural-tests
   set-theoretic-type-tests
   struct-tests
   poly-tests
   function-tests
   oo-tests
   values-tests
   other-tests))
