#lang racket/base

;; Tests for type, prop, object, etc. printers for Typed Racket

(require "test-utils.rkt"
         rackunit
         typed-racket/standard-inits
         typed-racket/tc-setup
         typed-racket/rep/type-rep
         typed-racket/rep/values-rep
         typed-racket/types/abbrev
         typed-racket/types/numeric-tower
         typed-racket/types/printer
         typed-racket/types/union
         typed-racket/utils/tc-utils
         (submod typed-racket/base-env/base-types initialize))

(provide tests)
(gen-test-main)

(define (prints-as? thing str)
  (string=? (format "~a" thing) str))

(define (prints-as*? thing strs)
  (let ([actual (format "~a" thing)])
    (for/or ([str (in-list strs)])
      (string=? actual str))))

(define (pretty-prints-as? thing str)
  (string=? (pretty-format-rep thing) str))

(define (pretty-prints-as*? thing strs)
  (let ([actual (pretty-format-rep thing)])
    (for/or ([str (in-list strs)])
      (string=? actual str))))

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
    (check-prints-as? (make-App (-poly (a) (-lst a)) (list -Nat) #'foo)
                      "(Listof Nonnegative-Integer)")
    (check-prints-as? (make-Mu 'x (Un -Null (-pair -Nat (make-F 'x))))
                      "(Listof Nonnegative-Integer)")
    (check-prints-as? (-lst* -String -Symbol) "(List String Symbol)")

    ;; next three cases for PR 14552
    (check-true (prints-as*? (-mu x (Un (-pair x x) -Null))
                             '("(Rec x (U Null (Pairof x x)))" "(Rec x (U (Pairof x x) Null))")))
    (check-true
     (prints-as*? (-mu x (Un (-pair (-box x) x) -Null))
                  '("(Rec x (U Null (Pairof (Boxof x) x)))" "(Rec x (U (Pairof (Boxof x) x) Null))")))
    (check-true
     (prints-as*? (-mu x (Un (-mpair x x) -Null))
                  '("(Rec x (U Null (MPairof x x)))" "(Rec x (U (MPairof x x) Null))")))

    (check-prints-as? -Custodian "Custodian")
    (check-prints-as? (make-Opaque #'integer?) "(Opaque integer?)")
    (check-prints-as? (make-Vector -Nat) "(Vectorof Nonnegative-Integer)")
    (check-prints-as? (make-HeterogeneousVector (list -Symbol -String))
                      "(Vector Symbol String)")
    (check-prints-as? (-box (-val 3)) "(Boxof 3)")
    (check-prints-as? (make-Future -Void) "(Futureof Void)")
    (check-prints-as? (-> -String -Void) "(-> String Void)")
    (check-true
     (prints-as*? (Un -String -Void) '("(U String Void)" "(U Void String)")))
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
                      (string-append "(-> Any Path-String [#:exists (U 'error"
                                     " 'append 'update 'replace 'truncate"
                                     " 'truncate/replace)] [#:mode (U"
                                     " 'binary 'text)] Void)"))
    (check-prints-as? (-> Univ (-AnyValues -tt)) "(-> Any AnyValues)")
    (check-prints-as? (-> Univ (-AnyValues (-is-type '(0 . 0) -String)))
                      "(-> Any AnyValues : (String @ (0 0)))")
    (check-prints-as? (-AnyValues -tt) "AnyValues")
    (check-prints-as? (-AnyValues (-is-type '(0 . 0) -String))
                      "(AnyValues : (String @ (0 0)))")

    (check-prints-as? (->opt Univ [] -Void) "(-> Any Void)")
    (check-prints-as? (->opt [-String] -Void) "(->* () (String) Void)")
    (check-prints-as? (->opt Univ [-String] -Void) "(->* (Any) (String) Void)")
    (check-prints-as? (->opt Univ -Symbol [-String] -Void)
                      "(->* (Any Symbol) (String) Void)")
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
    (check-prints-as? (make-UnitTop) "UnitTop"))
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

