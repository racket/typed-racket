#lang racket/base

(require "test-utils.rkt" "evaluator.rkt"
         rackunit
         (only-in racket/contract contract?)
         syntax/srcloc syntax/location
         (for-syntax
           syntax/parse
           racket/base
           typed-racket/private/type-contract
           typed-racket/static-contracts/instantiate
           typed-racket/static-contracts/structures
           typed-racket/static-contracts/combinators
           typed-racket/static-contracts/combinators/name
           typed-racket/types/abbrev
           typed-racket/types/numeric-tower
           typed-racket/rep/type-rep
           typed-racket/env/type-name-env))

(provide tests)
(gen-test-main)

(begin-for-syntax
  (define-splicing-syntax-class return
    #:attributes (e sc)
    (pattern (~seq e:expr) #:attr sc #f)
    (pattern (~seq e:expr #:ret sc:expr)))

  (define-splicing-syntax-class type-enforcement-flag
    #:attributes (value)
    (pattern (~or #:deep
                  (~seq))
      #:with value 'deep)
    (pattern (~seq #:optional)
      #:with value 'optional)
    (pattern (~seq #:shallow)
      #:with value 'shallow)))

(define-syntax t/sc
  (syntax-parser
    [(_ r:return te-flag:type-enforcement-flag)
     (syntax/loc #'r.e
       (test-case
         (format "Conversion:~a" (quote-line-number r.e))
         (with-check-info (['type 'r.e]
                           ['location (build-source-location-list (quote-srcloc r.e))]
                           ['enforcement-mode 'te-flag.value])
           (phase1-phase0-eval
             (define sc
                (type->static-contract r.e (lambda (#:reason _) #f) #:enforcement-mode 'te-flag.value))
             (if sc
                 #`(begin
                     (~? (check-equal? #,sc #,r.sc))
                     (with-check-info (['static '#,sc])
                       (phase1-phase0-eval
                         (define ctc (cadr
                                       (instantiate '#,sc
                                         (lambda (#:reason _) (error "static-contract could not be converted to a contract")))))
                         #,#'#`(with-check-info (['contract '#,ctc])
                            (define runtime-contract #,ctc)
                            (check-pred contract? runtime-contract)))))
                 #'(fail-check "Type could not be converted to a static contract"))))))]))

(define-syntax t/fail
  (syntax-parser
    [(_ e:expr (~optional (~seq #:typed-side typed-side) #:defaults ([typed-side #'#t])) te-flag:type-enforcement-flag)
     #`(test-case (format "~a" 'e)
         (define sc
           (phase1-phase0-eval
             (let/ec exit
               #`'#,(type->static-contract e (lambda (#:reason _) (exit #'#f)) #:typed-side typed-side #:enforcement-mode 'te-flag.value))))
         (when sc
           (with-check-info (['static sc])
             (fail-check "Type was incorrectly converted to contract"))))]))

(define tests
  (test-suite "Conversion Tests"
   (test-suite "Guarded Tests"
    (t/sc (-Number . -> . -Number))
    (t/sc (cl->* (-> -Symbol)
                 (-Symbol . -> . -Symbol)))
    (t/sc (cl->* (-> -Symbol)
                 (-Symbol -Symbol . -> . -Symbol)))
    (t/sc (cl->* (-Symbol . -> . -Symbol)
                 (-Symbol -Symbol . -> . -Symbol)))
    (t/sc (-Promise -Number))
    (t/sc (-Promise -Bottom) #:ret (promise/sc (or/sc)))
    (t/sc (-struct-property (-> Univ -Number Univ) #f))
    (t/sc (-struct-property (-> -Self -Number) #f))
    (t/fail (-struct-property (-> -Self -Number) #f)  #:typed-side #f)
    (t/sc (-lst -Symbol))
    (t/sc -Boolean)
    (t/sc Univ)
    (t/sc (-set Univ))
    (t/sc (-poly (a) (-lst a)))
    (t/fail ((-poly (a) (-vec a)) . -> . -Symbol))
    (t/fail (-poly (a) (-lst a)) #:typed-side #f)
    (t/sc (-mu a (-lst a)))
    (t/sc (-mu a (-box a)))
    (t/sc (-mu sexp (Un -Null -Symbol (-pair sexp sexp) (-vec sexp) (-box sexp))))
    (t/sc (-mu a (-> a a)))
    (t/sc (-seq -Symbol))
    ;; HashTables with non-flat keys and values (Issue 625)
    ;;   https://github.com/racket/typed-racket/issues/625
    (t/sc (-Mutable-HT (-vec Ident) (-vec Ident))
      #:ret (mutable-hash/sc
              (vectorof/sc identifier?/sc)
              (vectorof/sc identifier?/sc)))
    (t/sc (-Immutable-HT (-vec Ident) (-vec Ident))
      #:ret (immutable-hash/sc
              (vectorof/sc identifier?/sc)
              (vectorof/sc identifier?/sc)))
    (t/sc (-Weak-HT (-vec Ident) (-vec Ident))
      #:ret (weak-hash/sc
              (vectorof/sc identifier?/sc)
              (vectorof/sc identifier?/sc)))
    (t/sc (-HT (-vec Ident) (-vec Ident))
      #:ret (hash/sc
              (vectorof/sc identifier?/sc)
              (vectorof/sc identifier?/sc)))
    ;; Unions of hash types should merge into one hash?/sc
    (t/sc (Un (-HT Ident Ident) (-Mutable-HT Ident Ident))
      #:ret (hash/sc identifier?/sc identifier?/sc))
    (t/sc (Un (-Weak-HT Ident Ident) (-Weak-HT -Bottom -Bottom)
              (-Immutable-HT Ident Ident)
              (-Mutable-HT -Bottom -Bottom) (-Mutable-HT Ident Ident))
      #:ret (or/sc (hash/sc identifier?/sc identifier?/sc)
                   (hash/sc (or/sc) (or/sc))))
    (t/sc (Un -Weak-HashTableTop
              (-Weak-HT Ident Ident)
              (-Mutable-HT Ident Ident)
              -Mutable-HashTableTop
              (-Immutable-HT Univ Univ))
      #:ret (and/sc hash?/sc any-wrap/sc))
    ;; Vectors
    (t/sc (-mvec Ident)
      #:ret (mutable-vectorof/sc identifier?/sc))
    (t/sc (-ivec Ident)
      #:ret (immutable-vectorof/sc identifier?/sc))
    (t/sc (-vec Ident)
      #:ret (vectorof/sc identifier?/sc))
    ;; Hetero vectors
    (t/sc (-mvec* Ident Ident)
      #:ret (mutable-vector/sc identifier?/sc identifier?/sc))
    (t/sc (-ivec* Ident Ident)
      #:ret (immutable-vector/sc identifier?/sc identifier?/sc))
    (t/sc (-vec* Ident Ident)
      #:ret (vector/sc identifier?/sc identifier?/sc))
    ;; Unions of vectors types should merge into one
    (t/sc (Un (-ivec Ident) (-mvec -Bottom) (-mvec Ident))
      #:ret (or/sc (vectorof/sc identifier?/sc) (mutable-vectorof/sc (or/sc))))
    (t/sc (Un (-ivec Ident) (-mvec Ident)
              (-ivec (-box Ident)) (-mvec (-box Ident)))
      #:ret (or/sc (vectorof/sc (box/sc identifier?/sc))
                   (vectorof/sc identifier?/sc)))
    (t/sc (Un (-ivec Ident)
              -Mutable-VectorTop (-ivec Univ))
      #:ret (and/sc vector?/sc any-wrap/sc))
    ;; Unions of hetero vectors
    (t/sc (Un (-ivec* Ident) (-mvec* -Bottom) (-mvec* Ident))
      #:ret (or/sc (vector/sc identifier?/sc) (mutable-vector/sc (or/sc))))
    (t/sc (Un (-ivec* Ident Ident Ident) (-mvec* Ident Ident Ident)
              (-ivec* (-box Ident)) (-mvec* (-box Ident)))
      #:ret (or/sc (vector/sc (box/sc identifier?/sc))
                   (vector/sc identifier?/sc identifier?/sc identifier?/sc)))
    (t/sc (Un (-ivec* Ident)
              -Mutable-VectorTop
              (-ivec* Univ))
      #:ret (or/sc (and/sc mutable-vector?/sc any-wrap/sc)
                   (immutable-vector/sc (and/sc any/sc any-wrap/sc))))
    ;; Unions of hashes and vectors
    (t/sc (Un
            -Mutable-VectorTop
            (-Immutable-HT -Bottom -Bottom)
            (-Immutable-HT Ident Ident)
            (-Mutable-HT -Bottom -Bottom)
            (-Weak-HT Ident Ident)
            (-ivec Univ)
            (-ivec* Ident Ident Ident)
            (-mvec* Ident Ident Ident))
      #:ret (or/sc (hash/sc identifier?/sc identifier?/sc)
                   (mutable-hash/sc (or/sc) (or/sc))
                   (and/sc vector?/sc any-wrap/sc)))
    ;; These tests for unit static contracts are insufficient, but
    ;; in order to test Unit types the signature environment must be
    ;; set up correctly. More complex cases of compilation to unit/c
    ;; contracts are tested by integration tests.
    (t/sc (-unit null null null (-values (list -String))))
    (t/sc (-unit null null null (-values (list -Symbol -String))))
    (t/fail (-unit null null null ManyUniv)))

   (test-suite "Shallow Tests"
    (t/sc (-Number . -> . -Number) #:shallow)
    (t/sc (cl->* (-> -Symbol)
                 (-Symbol . -> . -Symbol)) #:shallow)
    (t/sc (-Promise -Number) #:ret promise?/sc #:shallow)
    (t/sc (-struct-property (-> -Self -Number) #f) #:ret struct-type-property?/sc #:shallow)
    (t/sc (-struct-property (-> Univ -Number Univ) #f) #:ret struct-type-property?/sc #:shallow)
    (t/sc (-set Univ) #:ret set?/sc #:shallow)
    (t/sc ((-poly (a) (-vec a)) . -> . -Symbol) #:shallow)
    (t/sc (-poly (a) (-lst a)) #:ret list?/sc #:shallow)
    (t/sc (-mu sexp (Un -Null -Symbol (-pair sexp sexp) (-vec sexp) (-box sexp))) #:shallow)
    (t/sc (-Mutable-HT (-vec -Symbol) (-vec -Symbol)) #:shallow)
    (t/sc (-HT (-vec -Symbol) (-vec -Symbol)) #:shallow)
    (t/sc (-unit null null null (-values (list -Symbol -String))) #:shallow)
    (t/sc (-unit null null null ManyUniv) #:shallow))

   (test-suite "Optional (typing) tests"
    (t/sc (-Number . -> . -Number) #:ret any/sc #:optional)
    (t/sc (-struct-property (-> -Self -Number) #f) #:ret any/sc #:optional)
    (t/sc (-struct-property (-> -Self -Number) #f) #:ret any/sc  #:optional)
    (t/sc (-set Univ) #:ret any/sc #:optional)
    (t/sc (-HT (-vec -Symbol) (-vec -Symbol)) #:ret any/sc #:optional)
    (t/sc (-unit null null null ManyUniv) #:ret any/sc #:optional))

   ;; When a Name type resolves to a Mu, the name-def SC should be a
   ;; plain object/sc, not wrapped in recursive-sc.  The single level of
   ;; recursive-contract comes from extra-defs in instantiate; verifying
   ;; that the name-def is not itself a recursive-sc ensures no doubling.
   (test-suite "Name->Mu produces no double recursive-sc"
    (test-case "recursive class type alias"
      (phase1-phase0-eval (define name-id (datum->syntax #f 'TestC%))
                          (define the-type (make-Name name-id 0 #f))
                          (register-type-name name-id (-mu a (-object #:method [(m (-> a -Void))])))
                          (with-new-name-tables
                           (let ()
                             (define sc (type->static-contract the-type (lambda (#:reason _) #f)))
                             ;; The top-level SC is just a name reference.
                             ;; Default typed-side=#t maps to 'typed.
                             (define typed-name-sc (lookup-name-sc the-type 'typed))
                             (define both-name-sc (lookup-name-sc the-type 'both))
                             ;; The name-def SC should be a plain object/sc.
                             ;; Before the fix, this was a recursive-sc wrapping
                             ;; the object/sc, which combined with the
                             ;; recursive-contract from extra-defs produced
                             ;; double nesting.
                             (define both-def (lookup-name-def the-type 'both))
                             (define expected-def
                               (object/sc #t
                                          (list (member-spec 'method
                                                             'm
                                                             (function/sc #t
                                                                          (list any/sc both-name-sc)
                                                                          null
                                                                          null
                                                                          null
                                                                          #f
                                                                          (list (flat/sc #'void?)))))))
                             #`(begin
                                 ;; The top-level result is a name-sc reference
                                 (check-equal? '#,sc '#,typed-name-sc)
                                 ;; The name-def is object/sc, not recursive-sc
                                 (check-equal? '#,both-def '#,expected-def)))))))
   ))
