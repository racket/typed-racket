#lang racket/base

;; Tests for type, prop, object, etc. printers for Typed Racket

(require "test-utils.rkt"
         rackunit
         racket/match
         typed-racket/standard-inits
         typed-racket/tc-setup
         typed-racket/rep/type-rep
         typed-racket/rep/values-rep
         typed-racket/types/abbrev
         typed-racket/types/prop-ops
         typed-racket/types/numeric-tower
         typed-racket/types/printer
         typed-racket/utils/tc-utils
         (submod typed-racket/base-env/base-types initialize))

(provide tests)
(gen-test-main)

(define y #'y)
(define z #'z)

(define (prints-as? thing expected)
  (cond
    [(string? expected) (string=? (format "~a" thing) expected)]
    [(procedure? expected) (expected (format "~a" thing))]
    [else (error 'prints-as? "unsupported expected ~a" expected)]))

(define (pretty-prints-as? thing str)
  (string=? (pretty-format-rep thing) str))

(define-binary-check (check-prints-as? prints-as? actual expected))
(define-binary-check (check-pretty-prints-as? pretty-prints-as? actual expected))

;; Using `do-standard-inits` here does not work when the file is invoked
;; individually (rather than through the test harness)
(initialize-type-names)
(current-type-names (init-current-type-names))

(define tests
  (test-suite
   "Printing tests"
   (test-suite
    "Type printing tests"
    (check-prints-as? (-val 3) "3")
    (check-prints-as? (-val 'a) "'a")
    (check-prints-as? (-val #\a) "#\\a")
    (check-prints-as? Univ "Any")
    (check-prints-as? (Un (-val #t) (-val #f)) "Boolean")
    (check-prints-as? (-lst -Nat) "(Listof Nonnegative-Integer)")
    (check-prints-as? (make-App (-poly (a) (-lst a)) (list -Nat))
                      "(Listof Nonnegative-Integer)")
    (check-prints-as? (make-Mu 'x (Un -Null (-pair -Nat (make-F 'x))))
                      "(Listof Nonnegative-Integer)")
    (check-prints-as? (-lst* -String -Symbol) "(List String Symbol)")

    ;; next three cases for PR 14552
    (check-prints-as? (-mu x (Un (-pair x x) -Null))
                      "(Rec x (U (Pairof x x) Null))")
    (check-prints-as? (-mu x (Un (-pair (-box x) x) -Null))
                      "(Rec x (U (Pairof (Boxof x) x) Null))")
    (check-prints-as? (-mu x (Un (-mpair x x) -Null))
                      "(Rec x (U (MPairof x x) Null))")

    (check-prints-as? -Custodian "Custodian")
    (check-prints-as? (make-Opaque #'integer?) "(Opaque integer?)")
    (check-prints-as? (make-Vector -Nat) "(Vectorof Nonnegative-Integer)")
    (check-prints-as? (make-HeterogeneousVector (list -Symbol -String))
                      "(Vector Symbol String)")
    (check-prints-as? (-box (-val 3)) "(Boxof 3)")
    (check-prints-as? (make-Future -Void) "(Futureof Void)")
    (check-prints-as? (-> -String -Void) "(-> String Void)")
    (check-prints-as? (Un -String -Void) "(U String Void)")
    (check-prints-as? (-pair -String -Void) "(Pairof String Void)")
    (check-prints-as? (make-ListDots -Boolean 'x) "(List Boolean ... x)")
    (check-prints-as? (make-F 'X) "X")
    (check-prints-as? (make-Values (list (-result -String) (-result -Symbol)))
                      "(values String Symbol)")
    (check-prints-as? (make-ValuesDots (list (-result -String) (-result -Symbol)) (make-F 'x) 'x)
                      "(values String Symbol x ... x)")
    (check-prints-as? (-polydots (a b) (->... (list a) (b b) a))
                      "(All (a b ...) (-> a b ... b a))")
    (check-prints-as? (-mu x (-lst x)) "(Rec x (Listof x))")
    (check-prints-as? (-seq -String -Symbol) "(Sequenceof String Symbol)")
    (check-prints-as? (-poly (a) (-> a -Void)) "(All (a) (-> a Void))")
    (check-prints-as? (-> -Input-Port (make-Values (list (-result -String -true-propset)
                                                         (-result -String -true-propset))))
                      "(-> Input-Port (values (String : (Top | Bot)) (String : (Top | Bot))))")
    (check-prints-as? (make-pred-ty -String)
                      "(-> Any Boolean : String)")
    (check-prints-as? (asym-pred Univ -Boolean (-PS (-is-type 0 -String) -tt))
                      "(-> Any Boolean : #:+ String)")
    (check-prints-as? (-> Univ Univ -Boolean : (-PS (-is-type 0 -String) -tt))
                      "(-> Any Any Boolean)")
    (check-prints-as? (-> Univ Univ -Boolean : (-PS (-is-type 1 -String) -tt))
                      "(-> Any Any Boolean)")
    ;; PR 14510 (next three tests)
    (check-prints-as? (-> Univ (-> Univ -Boolean : (-PS (-is-type '(1 . 0) -String)
                                                        (-not-type '(1 . 0) -String))))
                      "(-> Any (-> Any Boolean))")
    (check-prints-as? (-> Univ Univ -Boolean : (-PS (-is-type '(0 . 1) -String)
                                                    (-not-type '(0 . 1) -String)))
                      "(-> Any Any Boolean)")
    (check-prints-as? (-> Univ Univ -Boolean : (-PS (-is-type '(0 . 0) -String)
                                                    (-not-type '(0 . 0) -String)))
                      "(-> Any Any Boolean)")
    (check-prints-as? (-> Univ (make-Values (list (-result -String -tt-propset -empty-obj)
                                                  (-result -String -tt-propset -empty-obj))))
                      "(-> Any (values String String))")
    ;; this case tests that the Number union is printed with its name
    ;; rather than its expansion (a former bug)
    (check-prints-as? (->* '() -Number -Void) "(-> Number * Void)")
    (check-prints-as? (->key Univ -Pathlike
                             #:exists
                             (one-of/c 'error 'append 'update 'replace
                                       'truncate 'truncate/replace)
                             #f
                             #:mode
                             (one-of/c 'binary 'text)
                             #f
                             -Void)
                      (string-append "(-> Any Path-String [#:exists (U 'append"
                                     " 'error 'replace 'truncate 'truncate/replace"
                                     " 'update)] [#:mode (U"
                                     " 'binary 'text)] Void)"))
    (check-prints-as? (-> Univ (-AnyValues -tt)) "(-> Any AnyValues)")
    (check-prints-as? (-> Univ (-AnyValues (-is-type '(0 . 0) -String)))
                      "(-> Any AnyValues : (: (0 0) String))")
    (check-prints-as? (-AnyValues -tt) "AnyValues")
    (check-prints-as? (-AnyValues (-is-type '(0 . 0) -String))
                      "(AnyValues : (: (0 0) String))")

    (check-prints-as?
     (make-Fun (list (-Arrow (list Univ) #:rest Univ -String)
                     (-Arrow (list Univ -String) #:rest Univ -String)))
     ;; NOT (->* (Any) (String) #:rest Any String)
     "(case-> (-> Any Any * String) (-> Any String Any * String))")
    (check-prints-as? (->opt Univ [] -Void) "(-> Any Void)")
    (check-prints-as? (->opt [-String] -Void) "(->* () (String) Void)")
    (check-prints-as? (->opt Univ [-String] -Void) "(->* (Any) (String) Void)")
    (check-prints-as? (->opt Univ -Symbol [-String] -Void)
                      "(->* (Any Symbol) (String) Void)")
    (check-prints-as? (->optkey Univ [-String] #:rest -Symbol -Void)
                      "(->* (Any) (String) #:rest Symbol Void)")
    (check-prints-as? (->optkey Univ [-String] #:x -String #f -Void)
                      "(->* (Any) (String #:x String) Void)")
    (check-prints-as? (->optkey Univ [-String] #:x -String #t -Void)
                      "(->* (Any #:x String) (String) Void)")
    (check-prints-as? (->optkey Univ [-String] #:x -String #t -Void)
                      "(->* (Any #:x String) (String) Void)")
    (check-prints-as? (->optkey Univ [-String] #:rest -String #:x -String #t -Void)
                      "(->* (Any #:x String) (String) #:rest String Void)")
    (check-prints-as? (cl->* (->opt -Pathlike [-String] -Void)
                             (->optkey Univ [-String] #:rest -String #:x -String #t -Void))
                      (string-append "(case-> (->* (Path-String) (String) Void) "
                                     "(->* (Any #:x String) (String) #:rest String Void))"))
    (check-prints-as? (make-Unit null null null (-values (list -String)))
                      "(Unit (import) (export) (init-depend) String)")
    ;; Setup for slightly more complex unit printing test
    (let* ([a^ (make-Signature #'a^ #f null)]
           [a-sub^ (make-Signature #'a-sub^ #'a^ (list (cons #'a -String)))]
           [b^ (make-Signature #'b^ #f (list (cons #'b -Integer)))]
           [c^ (make-Signature #'c^ #f (list (cons #'c -Symbol)))]
           [d^ (make-Signature #'d^ #f (list (cons #'d -String)))])
      (check-prints-as? (make-Unit (list a^) null null (-values (list -String)))
                        "(Unit (import a^) (export) (init-depend) String)")
      (check-prints-as? (make-Unit (list a^) (list b^) null (-values (list -String)))
                        "(Unit (import a^) (export b^) (init-depend) String)")
      (check-prints-as? (make-Unit (list a^) (list b^) (list a^) (-values (list -String)))
                        "(Unit (import a^) (export b^) (init-depend a^) String)")
      (check-prints-as? (make-Unit (list a-sub^) null null (-values (list -String)))
                        "(Unit (import a-sub^) (export) (init-depend) String)")
      (check-prints-as? (make-Unit (list a-sub^) null (list a-sub^)  (-values (list -String)))
                        "(Unit (import a-sub^) (export) (init-depend a-sub^) String)")
      (check-prints-as? (make-Unit null (list a-sub^) null (-values (list -String)))
                        "(Unit (import) (export a-sub^) (init-depend) String)")
      (check-prints-as? (make-Unit (list a^ b^) (list c^ d^) null (-values (list -String)))
                        "(Unit (import a^ b^) (export c^ d^) (init-depend) String)")
      (check-prints-as? (make-Unit (list a^ b^) null null (-values (list -String)))
                        "(Unit (import a^ b^) (export) (init-depend) String)")
      (check-prints-as? (make-Unit null (list c^ d^) null (-values (list -String)))
                        "(Unit (import) (export c^ d^) (init-depend) String)")
      (check-prints-as? (make-Unit (list a^ b^) (list c^ d^) (list b^) (-values (list -String)))
                        "(Unit (import a^ b^) (export c^ d^) (init-depend b^) String)")
      (check-prints-as? (make-Unit (list a^ b^) (list c^ d^) (list b^ a^) (-values (list -String)))
                        "(Unit (import a^ b^) (export c^ d^) (init-depend b^ a^) String)"))
    (check-prints-as? -UnitTop "UnitTop")
    (check-prints-as? (-refine/fresh x -Int (-leq (-lexp x) (-lexp 42)))
                      (λ (str) (match (read (open-input-string str))
                                 [`(Refine [,(? symbol? x) : Integer]
                                           (<= ,(? symbol? x) 42)) #t]
                                 [_ #f])))
    (check-prints-as? (-refine/fresh x (-pair -Int -Int) (-leq (-lexp (-car-of (-id-path x)))
                                                               (-lexp (-cdr-of (-id-path x)))))
                      (λ (str) (match (read (open-input-string str))
                                 [`(Refine [,x : (Pairof Integer Integer)] (<= (car ,x) (cdr ,x))) #t]
                                 [_ #f])))
    (check-prints-as? (-refine/fresh x (-vec Univ) (-leq (-lexp (-vec-len-of (-id-path x)))
                                                         (-lexp 42)))
                      (λ (str) (match (read (open-input-string str))
                                 [`(Refine [,x : (Vectorof Any)] (<= (vector-length ,x) 42)) #t]
                                 [_ #f])))
    (check-prints-as? (-refine/fresh x -Int (-and (-leq (-lexp x) (-lexp 42))
                                                  (-leq (-lexp 42) (-lexp x))
                                                  (-leq (-lexp 1 x) (-lexp #'y))))
                      (λ (str) (match (read (open-input-string str))
                                 [`(Refine [,x : Integer] (and (= ,x 42) (< ,x y))) #t]
                                 [`(Refine [,x : Integer] (and (= 42 ,x) (< ,x y))) #t]
                                 [`(Refine [,x : Integer] (and (< ,x y) (= ,x 42))) #t]
                                 [`(Refine [,x : Integer] (and (< ,x y) (= ,x 42))) #t]
                                 [_ #f])))
    (check-prints-as? (-refine/fresh x -Int (-and (-leq (-lexp x) (-lexp 42))
                                                  (-leq (-lexp 42) (-lexp x))))
                      (λ (str) (match (read (open-input-string str))
                                 [`(Refine [,x : Integer] (= ,x 42)) #t]
                                 [`(Refine [,x : Integer] (= 42 ,x)) #t]
                                 [_ #f])))
    (check-prints-as? (-refine/fresh x -Int (-eq (-lexp x) (-lexp (list 1 #'y) 42)))
                      (λ (str) (match (read (open-input-string str))
                                 [`(Refine [,x : Integer] (= ,x (+ y 42))) #t]
                                 [`(Refine [,x : Integer] (= ,x (+ 42 y))) #t]
                                 [`(Refine [,x : Integer] (= (+ y 42) ,x)) #t]
                                 [`(Refine [,x : Integer] (= (+ 42 y) ,x)) #t]
                                 [_ #f])))
    (check-prints-as? (-refine/fresh x -Int (-eq (-lexp x) (-lexp (list 1 #'y) 42)))
                      (λ (str) (match (read (open-input-string str))
                                 [`(Refine [,x : Integer] (= ,x (+ y 42))) #t]
                                 [`(Refine [,x : Integer] (= (+ y 42) ,x)) #t]
                                 [`(Refine [,x : Integer] (= ,x (+ 42 y))) #t]
                                 [`(Refine [,x : Integer] (= (+ 42 y) ,x)) #t]
                                 [_ #f])))
    (check-prints-as? (-refine/fresh x -Int (-eq (-lexp x) (-lexp (list -1 #'y) 42)))
                      (λ (str) (match (read (open-input-string str))
                                 [`(Refine [,x : Integer] (= ,x (- 42 y))) #t]
                                 [`(Refine [,x : Integer] (= (- 42 y) ,x)) #t]
                                 [_ #f])))
    (check-prints-as? (-refine/fresh x -Int (-eq (-lexp x) (-lexp (list 1 #'y) -42)))
                      (λ (str) (match (read (open-input-string str))
                                 [`(Refine [,x : Integer] (= ,x (- y 42))) #t]
                                 [`(Refine [,x : Integer] (= (- y 42) ,x)) #t]
                                 [_ #f])))
    (check-prints-as? (-refine/fresh x -Int (-is-type #'y -Int))
                      (λ (str) (match (read (open-input-string str))
                                 [`(Refine [,x : Integer] (: y Integer)) #t]
                                 [_ #f])))
    (check-prints-as? (-refine/fresh x -Int (-not-type #'y -Int))
                      (λ (str) (match (read (open-input-string str))
                                 [`(Refine [,x : Integer] (! y Integer)) #t]
                                 [_ #f])))
    (check-prints-as? (-refine/fresh x -Int (-or (-is-type #'y -Int)
                                                 (-is-type #'z -String)))
                      (λ (str) (match (read (open-input-string str))
                                 [`(Refine [,x : Integer] (or (: y Integer) (: z String))) #t]
                                 [`(Refine [,x : Integer] (or (: z String) (: y Integer))) #t]
                                 [_ #f])))
    (check-prints-as? (-unsafe-intersect (-val 2) (-v A)) "(∩ 2 A)")
    (check-prints-as?
     (dep-> ((x : -Int) (y : -Int))
            #:pre (-lt (-lexp (-id-path x)) (-lexp (-id-path y)))
            -Int)
     (λ (str)
       (match (read (open-input-string str))
         [`(-> ((,(? symbol? p) : Integer)
                (,(? symbol? q) : Integer))
               #:pre (,q ,p) (< ,p ,q)
               Integer)
          #t]
         [_ #f])))
    (check-prints-as?
     (dep-> ((x : -Int)
             (y : (-refine/fresh n -Int (-leq (-lexp (-id-path x)) (-lexp (-id-path n))))))
            -Int)
     (λ (str)
       (match (read (open-input-string str))
         [`(-> ((,(? symbol? r) : Integer)
                (,(? symbol? s) : (,r) (Refine (,t : Integer) (<= ,r ,t))))
               Integer)
          #t]
         [_ #f]))))

   (test-suite
    "Pretty printing tests"
    (check-pretty-prints-as? (-val 3) "3")
    (check-pretty-prints-as? (-val 'a) "'a")
    (check-pretty-prints-as? (-val #\a) "#\\a")
    (check-pretty-prints-as? Univ "Any")
    (check-pretty-prints-as? (Un (-val #t) (-val #f)) "Boolean")
    (check-pretty-prints-as? (-lst -Nat) "(Listof Nonnegative-Integer)")
    (check-pretty-prints-as?
     (-polydots (c a b)
       (cl->*
        (-> (-> a c) (-pair a (-lst a)) (-pair c (-lst c)))
        ((list
          ((list a) (b b) . ->... . c)
          (-lst a))
         ((-lst b) b) . ->... .(-lst c))))
     (string-append "(All (c a b ...)\n"
                    "  (case->\n"
                    "   (-> (-> a c) (Pairof a (Listof a)) (Pairof c (Listof c)))\n"
                    "   (-> (-> a b ... b c) (Listof a) (Listof b) ... b (Listof c))))"))
    (check-pretty-prints-as?
     (-poly (a) (cl->* (-> (-Syntax a) Univ Univ (-Syntax a))
                       (-> (-Syntax Univ) Univ Univ)))
     (string-append "(All (a)\n"
                    "  (case-> (-> (Syntaxof a) Any Any (Syntaxof a)) (-> (Syntaxof Any) Any Any)))")))))

