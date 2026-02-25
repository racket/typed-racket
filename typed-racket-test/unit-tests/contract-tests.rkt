#lang racket/base

(require "test-utils.rkt"
         (for-syntax racket/base
                     syntax/parse
                     syntax/srcloc)
         (for-template racket/base)
         typed-racket/private/type-contract
         typed-racket/rep/type-rep
         typed-racket/rep/values-rep
         typed-racket/types/abbrev
         typed-racket/types/numeric-tower
         typed-racket/types/prop-ops
         typed-racket/static-contracts/combinators
         typed-racket/static-contracts/optimize
         (submod typed-racket/private/type-contract numeric-contracts)
         (submod typed-racket/private/type-contract test-exports)
         (only-in racket/contract contract)
         racket/match
         (except-in typed/racket/class private)
         syntax/id-set
         rackunit)
(provide tests)
(gen-test-main)

(begin-for-syntax
  (define-splicing-syntax-class type-enforcement-flag
    #:attributes (value)
    (pattern (~or #:deep
                  (~seq))
      #:with value 'deep)
    (pattern (~seq #:optional)
      #:with value 'optional)
    (pattern (~seq #:shallow)
      #:with value 'shallow)))


;; (t ty [te-flag #:deep])
;; Convert type to a contract using the type enforcement mode named by `te-flag`
(define-syntax (t stx)
  (syntax-parse stx
   [(_ e te-flag:type-enforcement-flag)
    #'(test-case (format "~a" 'e)
        (let ([v e])
          (with-check-info (('type v) ('enforcement-mode 'te-flag.value))
            (type->contract
              e
              (λ (#:reason [reason #f])
                (fail-check (or reason "Type could not be converted to contract")))
              #:enforcement-mode 'te-flag.value))))]))


;; (t-sc ty sc [te-mode #:deep])
;; Convert `ty` to an optimized static contract, check equal to `sc`
(define-syntax (t-sc stx)
  (syntax-parse stx
   [(_ e-t e-sc te-flag:type-enforcement-flag)
    #'(test-case (format "~a" '(e-t -> e-sc))
       (let ([t e-t] [sc e-sc])
         (with-check-info (['type t] ['expected sc] ['enforcement-mode 'te-flag.value])
           (define actual
             (optimize
               (type->static-contract
                 t
                 (λ (#:reason [reason #f])
                   (fail-check (or reason "Type could not be converted to contract")))
                 #:enforcement-mode 'te-flag.value)))
           (with-check-info (['actual actual])
             (unless (equal? actual sc)
               (fail-check "Static contract didn't match expected"))))))]))


;; (t/fail ty reason [te-flag #:deep])
;; Try converting `ty` to a static contract, but expect an error message that contains `reason`
(define-syntax (t/fail stx)
  (syntax-parse stx
   [(_ e expected-reason te-flag:type-enforcement-flag)
    #'(test-case (format "~a" 'e)
        (let ((v e))
          (with-check-info (('expected expected-reason)
                            ('type v)
                            ('enforcement-mode 'te-flag.value))
            (define reason
              (let/ec exit
                (let ([contract (type->contract v (λ (#:reason [reason #f])
                                                     (exit (or reason "No reason given")))
                                                #:enforcement-mode 'te-flag.value)])
                  (match-define (list ctc-defs ctc) contract)
                  (define ctc-data (map syntax->datum (append ctc-defs (list ctc))))
                  (with-check-info (('contract ctc-data))
                    (fail-check "type could be converted to contract")))))
            (unless (regexp-match? expected-reason reason)
              (with-check-info (('reason reason))
                (fail-check "Reason didn't match expected."))))))]))

;; construct a namespace for use in typed-untyped interaction tests
(define (ctc-namespace)
  (parameterize ([current-namespace (make-base-namespace)])
    (namespace-require 'racket/contract)
    (namespace-require 'racket/sequence)
    (namespace-require 'racket/async-channel)
    (namespace-require 'typed-racket/utils/any-wrap)
    (namespace-require 'typed-racket/utils/evt-contract)
    (namespace-require 'typed-racket/utils/hash-contract)
    (namespace-require 'typed-racket/utils/opaque-object)
    (namespace-require 'typed-racket/utils/simple-result-arrow)
    (namespace-require 'typed-racket/utils/vector-contract)
    (namespace-require '(submod typed-racket/private/type-contract predicates))
    (namespace-require 'typed/racket/class)
    (current-namespace)))

;; (t-int type (-> any any) any)
;; Use #:typed (default) to simulate typed export,
;; #:untyped for untyped export.
(define-syntax-rule (t-int arg ...)
  (t-int/check arg ... check-not-exn))

(define (check-re re loc)
  (λ (thunk)
    (with-check-info* (list (make-check-location loc))
                      (lambda ()
      (check-exn
       (λ (e)
         (and (exn:fail? e)
              (regexp-match? re (exn-message e))))
       thunk)))))

;; (t-int/fail type (-> any any) any #:msg regexp)
;; Like t-int, but checks failing cases. Takes a regexp for checking
;; the exception message.
(define-syntax (t-int/fail stx)
  (syntax-parse stx
   [(_ arg ... #:msg re)
    (with-syntax ([loc (build-source-location-list stx)])
      (quasisyntax/loc stx
        (t-int/check arg ...  (check-re re 'loc))))]))

;; (t-int/check ty fn val ty-side [te-mode #:deep] check)
;; Convert `ty` to a contract, apply to `val`, then call `fn` on the result.
;; Both `ty-side` and `ty-mode` control the contract generation.
;; The whole computation runs in the context of `check`.
(define-syntax (t-int/check stx)
  (syntax-parse stx
    [(_ type-expr fun-expr val-expr
        (~or (~and (~or (~seq) (~seq #:typed))
                   (~bind [typed-side #'#t]))
             (~and (~seq #:untyped)
                   (~bind [typed-side #'#f])))
        te-flag:type-enforcement-flag
        check)
     (define-values [pos neg]
       (if (syntax-e #'typed-side)
         (values 'typed 'untyped)
         (values 'untyped 'typed)))
     #`(test-case (format "~a for ~a in ~a" 'type-expr 'val-expr 'fun-expr)
         (let ([type-val type-expr])
           (with-check-info (['type type-val] ['test-value (quote val-expr)] ['enforcement-mode 'te-flag.value])
             (define ctc-result
               (type->contract type-val
                               #:typed-side typed-side
                               (λ (#:reason [reason #f])
                                 (fail-check (or reason "Type could not be converted to contract")))
                               #:cache #f
                               #:enforcement-mode 'te-flag.value))
             (match-define (list extra-stxs ctc-stx) ctc-result)
             (define namespace (ctc-namespace))
             (define val (eval (quote val-expr) namespace))
             (define fun-val (eval (quote fun-expr) namespace))
             #,(quasisyntax/loc stx
                 (check (λ ()
                          (define ctced-val
                            (eval #`(let ()
                                      #,@(map (λ (stx) (syntax-shift-phase-level stx 1))
                                              extra-stxs)
                                      (contract #,(syntax-shift-phase-level ctc-stx 1)
                                                #,val
                                                #,(quote (quote #,pos))
                                                #,(quote (quote #,neg))))
                                  namespace))
                          (fun-val ctced-val)))))))]))

(define tests
 (test-suite
  "Contract Tests"
  (test-suite
   "Guarded Tests"
   (t (-Number . -> . -Number))
   (t (-Promise -Number))
   (t (-set Univ))
   (t (make-pred-ty -Symbol))
   (t (->key -Symbol #:key -Boolean #t Univ))
   (t (make-Fun
       (list (-Arrow (list Univ) -Boolean #:kws (list (make-Keyword '#:key Univ #t))
                     #:props (-PS (-is-type 0 -Symbol) (-not-type 0 -Symbol))))))
   (t (-struct #'struct-name1 #f (list (make-fld -Symbol #'acc #f))))
   ;; Adapted from PR 13815
   (t (-poly (a) (-> a a)))
   (t (-poly (a) (-mu X (-> a X))))
   (t (-poly (a) (-poly (b) (-> a a))))
   ;; TODO enable the following test when type level lambdas are supported
   ;; (t (-poly (a) (-App (-poly (b) (-> a a)) (list -Number))))

   (t (-poly (a) -Flonum))
   (t (-poly (a) (-set -Number)))
   (t (-poly (a) (-lst a)))
   (t (-poly (a) (-vec a)))
   (t (-> (-poly (A B) (-> (Un A (-mu X (Un A (-lst X)))) (Un A (-mu X (Un A (-lst X))))))
          (-> -Symbol (-mu X (Un -Symbol (-lst X))))))

   (t (-polydots (a) -Symbol))
   (t (-polydots (a) (->... (list) (a a) -Symbol)))

   (t (-polyrow (a) (list null null null null) -Symbol))
   (t (-polyrow (a) (list null null null null)
                (-> (-class #:row (-v a)) (-class #:row (-v a)))))

   (t (-mu x (-Syntax x)))
   (t (-> (-> Univ -Bottom : -ff-propset) -Bottom : -ff-propset))
   (t (-poly (A B) (-> A B (Un A B))))
   (t (-> Univ (-some-res (X) (-> X -Boolean) : #:+ X)))
   (t (-some (X) (-> Univ (-> X -Boolean) : (-PS (-is-type 0 X)
                                                 -tt))))
   (t (->opt [-Number] (-> (make-Values (list)))))


   (t/fail ((-poly (a) (-vec a)) . -> . -Symbol)
           "cannot generate contract for non-function polymorphic type")
   (t/fail (-> (-poly (a b) (-> (Un a b) (Un a b))) Univ)
           "multiple parametric contracts are not supported")
   (t/fail
    (-> (-poly (A B) (-> (Un B (-mu X (Un A (-lst X)))) (Un B (-mu X (Un A (-lst X))))))
        (-> -Symbol (-mu X (Un -Symbol (-lst X)))))
    "multiple parametric contracts are not supported")
   (t/fail (-> (-polydots (a) (->... (list) (a a) -Symbol)) Univ)
           "cannot generate contract for variable arity polymorphic type")

   ;; PR 14894 - FIXME: the polydots case may be possible for typed functions
   (t/fail (-polydots (a) (->... (list) (a a) (make-ValuesDots null a 'a)))
           "dotted return values")
   (t/fail (-> ManyUniv)
           "unknown return values")

   ;; Github Issue #50
   (t (cl->* (-> -String -Bottom) (-> -String -Symbol -Bottom)))
   (t (make-Fun
       (list (-Arrow (list -String) -Boolean
                     #:kws (list (make-Keyword '#:key Univ #t))
                     #:props (-PS (-is-type 0 -Symbol) (-not-type 0 -Symbol)))
             (-Arrow (list -String Univ) -Boolean
                     #:kws (list (make-Keyword '#:key Univ #t))
                     #:props (-PS (-is-type 0 -Symbol) (-not-type 0 -Symbol))))))
   (t/fail (cl->* (-> -String ManyUniv) (-> -String Univ ManyUniv))
           "unknown return values")

   (t/fail
    (make-Fun
     (list (-Arrow (list) -Boolean #:kws (list (make-Keyword '#:key Univ #f)))
           (-Arrow (list Univ) -Boolean #:kws (list (make-Keyword '#:key2 Univ #f)))))
    "case function type with optional keyword arguments")
   (t/fail (-> (make-pred-ty -Symbol)-Symbol)
           "function type with props or objects")
   (t/fail (cl->*
            (-> -Boolean -Boolean)
            (-> -Symbol -Symbol))
           "two cases of arity 1")
   (t/fail (-struct #'struct-name2 #f (list (make-fld -Symbol #'acc #f)) (-> -Symbol))
           "procedural structs are not supported")
   (t/fail (-Syntax (-> -Boolean -Boolean))
           "required a flat contract but generated a chaperone contract")
   (t/fail (-Syntax (-seq -Boolean))
           "required a flat contract but generated an impersonator contract")
   (t/fail (-set (-seq -Boolean))
           "required a chaperone contract but generated an impersonator contract")

   (t/fail
    (make-Fun
     (list
      (-Arrow (list) -Boolean #:kws (list (make-Keyword '#:key Univ #t)))
      (-Arrow (list Univ Univ) -Boolean #:kws (list (make-Keyword '#:key2 Univ #t)))))
    "case function type with optional keyword arguments")
   (t/fail (-vec (-struct #'struct-name3 #f (list (make-fld (-seq -Symbol) #'acc #f)) #f #t))
           "required a chaperone contract but generated an impersonator contract")

   (t-sc -Number number/sc)
   (t-sc -Integer integer/sc)
   (t-sc (-lst Univ) (listof/sc any-wrap/sc))
   (t-sc (Un (-lst Univ) (-val #t)) (or/sc (flat/sc #''#t) (listof/sc any-wrap/sc)))
   (t-sc (Un (-val #f) (-val #t) (-lst (-val #f)))
         (or/sc (flat/sc #''#t) (flat/sc #''#f) (listof/sc (flat/sc #''#f))))
   (t-sc (-pair Univ Univ)
         (cons/sc any-wrap/sc any-wrap/sc))
   (t-sc -Null
         (flat/sc #''()))
   (t-sc (-pair Univ (-pair Univ -Null))
         (list/sc any-wrap/sc any-wrap/sc))
   (t-sc (-lst* -Symbol -String)
         (list/sc (flat/sc #'symbol?) (flat/sc #'string?)))
   (t-sc (-lst* -Number -Integer #:tail -Integer)
         (cons/sc number/sc (cons/sc integer/sc integer/sc)))

   (t-int Any-Syntax syntax? #'#'A #:typed) ;; GitHub issue #616

   ;; Github pull request #226
   (let ([ctc (-> Univ -Boolean)])
     ;; Ordinary functions should have a contract
     (t-int ctc
            (lambda (f) (f 6))
            (lambda (x) #t)
            #:untyped)
     (t-int/fail ctc
                 (lambda (f) (f 6))
                 (lambda (x) 'bad)
                 #:untyped
                 #:msg #rx"promised: boolean\\?.*produced: 'bad.*blaming: untyped")
     ;; Struct predicates should not have a contract
     (t-int ctc
            (lambda (foo?)
              (when (has-contract? foo?)
                (error "Regression failed for PR #266: struct predicate has a contract"))
              (foo? foo?))
            (let-values ([(_t _c foo? _a _m) (make-struct-type 'foo #f 0 0)])
              foo?)
            #:untyped)
     ;; Unless the struct predicate is deep by an untyped chaperone
     (t-int/fail ctc
                 (lambda (foo?) (foo? string-append))
                 (let-values ([(_t _c foo? _a _m) (make-struct-type 'foo #f 0 0)])
                   (chaperone-procedure foo? (lambda (x) (x 0) x)))
                 #:untyped
                 #:msg #rx"broke its own contract")
     ;; Typed chaperones are okay, though
     (t-int ctc
            (lambda (foo?)
              (when (has-contract? foo?)
                (error "Regression failed for PR #266: typed chaperone has a contract"))
              (foo? foo?))
            (let-values ([(_t _c foo? _a _m) (make-struct-type 'foo #f 0 0)])
              (chaperone-procedure foo? #f))
            #:typed))

   ;; Github pull request #714
   ;; - typed functions that return flat typed do not need chaperones
   ;; - typed functions that return non-flat types _do_ need chaperones,
   ;;   unless they happen to be struct-predicate-procedures
   (t-int (-> Univ -Boolean)
          (lambda (f)
            (when (has-contract? f)
              (raise-argument-error 'issue712-regression "(not/c has-contract?)" f)))
          ;; typed function returns flat value --- no need to chaperone
          (lambda (x) (string? x))
          #:typed)
   (t-int (-> Univ Univ)
          (lambda (f)
            (unless (has-contract? f)
              (raise-argument-error 'issue712-regression "has-contract?" f)))
          ;; typed function may return higher-order value --- need chaperone
          (lambda (x) (if (string? x) #true string?))
          #:typed)
   (t-int (-> Univ Univ)
          (lambda (actual-struct-predicate)
            (when (has-contract? actual-struct-predicate)
              (raise-argument-error 'pr714-regression "unchaperoned struct predicate" actual-struct-predicate)))
          (let ()
            (struct foo ())
            foo?)
          #:typed)
   (t-int (-> Univ Univ)
          (lambda (aliased-struct-predicate)
            (when (has-contract? aliased-struct-predicate)
              (raise-argument-error 'pr714-regression "unchaperoned struct predicate" aliased-struct-predicate)))
          (let ()
            (struct foo ())
            (define f foo?)
            f)
          #:typed)

   ;; classes
   (t-sc (-class) (class/sc #f null null))
   (t-sc (-class #:init ([x -Number #f] [y -Number #f]))
         (class/sc #f
                   (list (member-spec 'init 'x number/sc)
                         (member-spec 'init 'y number/sc))
                   null))
   (t-sc (-class #:init ([x -Number #f] [y -Number #t]))
         (class/sc #f
                   (list (member-spec 'init 'x number/sc)
                         (member-spec 'init 'y number/sc))
                   null))
   (t-sc (-class #:init ([x -Number #f]) #:init-field ([y -Integer #f]))
         (class/sc #f
                   (list (member-spec 'init 'x number/sc)
                         (member-spec 'init 'y integer/sc)
                         (member-spec 'field 'y integer/sc))
                   null))
   (t (-class #:method ([m (-poly (x) (-> x x))])))
   (t (-class #:method ([m (-polydots (x) (->... (list) (x x) -Void))])))
   (t (-class #:method ([m (-polyrow (x) (list null null null null)
                                     (-> (-class #:row (-v x)) -Void))])))

   ;; units
   ;; These tests do not have sufficient coverage because more
   ;; coverage requires a proper set up of the signature environment.
   ;; Further coverage of unit contract compilations occurs in
   ;; integration tests.
   (t-sc (-unit null null null (-values (list -Integer)))
         (unit/sc null null null (list integer/sc)))
   (t-sc (-unit null null null (-values (list -Integer -Number)))
         (unit/sc null null null (list integer/sc number/sc)))
   (t-sc (-unit null null null (-values (list)))
         (unit/sc null null null null))

   ;; typed/untyped interaction tests
   (t-int (-poly (a) (-> a a))
          (λ (f) (f 1))
          (λ (x) 1)
          #:typed)
   (t-int/fail (-poly (a) (-> a a))
               (λ (f) (f 1))
               (λ (x) 1)
               #:untyped
               #:msg #rx"produced: 1.*blaming: untyped")
   (t-int (cl->* (->* '() -String -String)
                 (->* (list -Symbol) -Symbol -Symbol))
          (λ (f) (f "a" "b"))
          (case-lambda [xs (car xs)]
                       [(sym . xs) sym]))
   (t-int (make-Evt -String)
          (λ (x) (channel-get x))
          (let ([ch (make-channel)])
            (thread (λ () (channel-put ch "ok")))
            ch)
          #:untyped)
   (t-int/fail (make-Evt -String)
               (λ (x) (channel-get x))
               (let ([ch (make-channel)])
                 (thread (λ () (channel-put ch 'bad)))
                 ch)
               #:untyped
               #:msg #rx"promised: string?.*produced: 'bad")
   (t-int/fail (make-Evt (-> -String -String))
               (λ (x) ((sync x) 'bad))
               (let ([ch (make-channel)])
                 (thread
                  (λ ()
                    (channel-put ch (λ (x) (string-append x "x")))))
                 ch)
               #:typed
               #:msg #rx"expected: string?.*given: 'bad")
   (t-int/fail (make-Evt -String)
               (λ (x) (channel-put x "bad"))
               (make-channel)
               #:untyped
               #:msg #rx"cannot put on a channel")
   (t-int (-vec -String)
          (λ (v) (string-length (vector-ref v 0)))
          (vector "astring")
          #:typed)
   (t-int (-HT -String -String)
          (λ (h) (string-length (hash-ref h "A")))
          (make-immutable-hash '(("A" . "B"))))
   ;; typed/untyped interaction with polymorphic datatypes
   ;;  (make sure `type-contract.rkt` merges contracts when needed)
   (t-int (-poly (a) (-> (-HT -String a) -Void))
          (λ (c) (c (make-immutable-hash '(("A" . 1)))))
          (λ (h) (void))
          #:untyped)
   (t-int (-poly (a) (-> (-vec a) -Void))
          (λ (c) (c (vector 2)))
          (λ (h) (void))
          #:untyped)
   (t-int (-poly (a) (-> (-vec (-pair a -String)) -Void))
          (λ (c) (c (vector 2)))
          (λ (h) (void))
          #:untyped)
   ;; TODO these tests fail, but should pass in a future Racket / Typed Racket with union contracts (not or/c)
   #;(t-int (-poly (a) (-> (Un (-HT -Boolean a) (-HT -String a)) -Void))
          (λ (c)
            (c (make-immutable-hash '((#true . 1))))
            (c (make-immutable-hash '(("A" . 1)))))
          (λ (h) (void))
          #:untyped)
   #;(t-int (-poly (a) (-> (Un (-vec* (-pair a -Symbol)) (-vec* (-pair a -String))) -Void))
          (λ (c)
            (c (vector (cons 2 'A)))
            (c (vector (cons 2 "B"))))
          (λ (h) (void))
          #:untyped)
   ;; typed/untyped interaction with class/object contracts
   (t-int/fail (-object #:method ([m (-> -String)]))
               (λ (o) (send o n))
               (new (class object% (super-new)
                      (define/public (m) "m")
                      (define/public (n) "n")))
               #:typed
               #:msg #rx"invoke only methods")
   (t-int (-class #:method ([m (-> -String)]))
          (λ (s%) (class s% (super-new)
                    (define/public (n) "ok")))
          (class object% (super-new)
            (define/public (m) "m"))
          #:untyped)

   ;; Github issue #368
   (t-int/fail (-> -Integer -Integer)
               values
               3
               #:untyped
               #:msg #rx"promised: a procedure")
   (t-int/fail (-> -Integer -Integer)
               values
               (λ () 3)
               #:untyped
               #:msg #rx"that accepts 1 non-keyword")

   ;; Value types with numbers shouldn't be checked with =
   (t-int/fail (make-Value 3.0)
               values
               3
               #:untyped
               #:msg #rx"promised: 3.0")
   (t-int/fail (make-Value 3)
               values
               3.0
               #:untyped
               #:msg #rx"promised: 3")

   ;; intersection types
   (t (-unsafe-intersect (-seq -Symbol) (-pair -Symbol (-lst -Symbol))))
   (t/fail (-unsafe-intersect (-Number . -> . -Number) (-String . -> . -String))
           "more than 1 non-flat contract")
   (t/fail (-unsafe-intersect (-box -Symbol) (-box Univ))
           "more than 1 non-flat contract")
   ;; logical refinements
   (let ([int<=42 (-refine/fresh x -Int (-leq (-lexp x) (-lexp 42)))])
     (t int<=42)
     (t-int (-> int<=42 int<=42)
            (λ (c) (c 1))
            (λ (_) 1)
            #:typed)
     (t-int/fail (-> int<=42 int<=42)
                 (λ (c) (c "bad"))
                 (λ (_) 1)
                 #:typed
                 #:msg #rx"expected: exact-integer?.*given: \"bad\"")
     (t-int/fail (-> int<=42 int<=42)
                 (λ (c) (c 43))
                 (λ (_) 1)
                 #:typed
                 #:msg #rx"expected: .*given: 43")
     (t-int/fail (-> int<=42 int<=42)
                 (λ (c) (c 1))
                 (λ (_) "bad")
                 #:untyped
                 #:msg #rx"promised: .*produced: \"bad\"")
     (t-int/fail (-> int<=42 int<=42)
                 (λ (c) (c 1))
                 (λ (_) 43)
                 #:untyped
                 #:msg #rx"promised: .*produced: 43")
     ;; make sure we optimize away the trusted side (e.g. if the type
     ;; system is unsound, we can return an incorrect value)
     (t-int (-> int<=42 int<=42)
            (λ (c) (c 1))
            (λ (_) 43)
            #:typed))
   ;; logical refinements w/ car/cdr
   (let ([ord-pair (-refine/fresh p (-pair -Int -Int) (-leq (-lexp (-car-of (-id-path p)))
                                                            (-lexp (-cdr-of (-id-path p)))))])
     (t ord-pair)
     (t-int (-> ord-pair ord-pair)
            (λ (c) (cons 1 2))
            (λ (_) 1)
            #:typed)
     (t-int/fail (-> ord-pair ord-pair)
                 (λ (c) (c "bad"))
                 (λ (_) (cons 1 2))
                 #:typed
                 #:msg #rx"expected: .*given: \"bad\"")
     (t-int/fail (-> ord-pair ord-pair)
                 (λ (c) (c (cons "a" 0)))
                 (λ (_) (cons 1 2))
                 #:typed
                 #:msg #rx"expected: .*given: \"a\"")
     (t-int/fail (-> ord-pair ord-pair)
                 (λ (c) (c (cons 0 "d")))
                 (λ (_) (cons 1 2))
                 #:typed
                 #:msg #rx"expected: .*given: \"d\"")
     (t-int/fail (-> ord-pair ord-pair)
                 (λ (c) (c (cons 1 0)))
                 (λ (_) (cons 1 2))
                 #:typed
                 #:msg #rx"expected: .*given: .*(1 . 0)")
     (t-int/fail (-> ord-pair ord-pair)
                 (λ (c) (c (cons 3 4)))
                 (λ (_) "bad")
                 #:untyped
                 #:msg #rx"promised: .*produced: \"bad\"")
     (t-int/fail (-> ord-pair ord-pair)
                 (λ (c) (c (cons 3 4)))
                 (λ (_) (cons 4 3))
                 #:untyped
                 #:msg #rx"promised: .*produced: .*(4 . 3)")
     ;; make sure we optimize away the trusted side (e.g. if the type
     ;; system is unsound, we can return an incorrect value)
     (t-int (-> ord-pair ord-pair)
            (λ (c) (cons 3 4))
            (λ (_) 43)
            #:typed))


   ;; refinements w/ vector length refinements
   (let ([vec-w/len<=10
          (-refine/fresh p (-vec Univ) (-leq (-lexp (-vec-len-of (-id-path p)))
                                             (-lexp 10)))])
     (t vec-w/len<=10)
     (t-int (-> vec-w/len<=10 vec-w/len<=10)
            (λ (c) (c (vector 0)))
            (λ (_) (vector 0))
            #:typed)
     (t-int/fail (-> vec-w/len<=10 vec-w/len<=10)
                 (λ (c) (c (for/vector ([n (in-range 11)]) n)))
                 (λ (_) (vector 0))
                 #:typed
                 #:msg #rx"expected: .*given: .*(0 1 2 3 4 5 6 7 8 9 10)")
     (t-int/fail (-> vec-w/len<=10 vec-w/len<=10)
                 (λ (c) (c (vector 0)))
                 (λ (_) (for/vector ([n (in-range 11)]) n))
                 #:untyped
                 #:msg #rx"promised: .*produced: .*(0 1 2 3 4 5 6 7 8 9 10)")
     ;; typed can be unsound
     (t-int (-> vec-w/len<=10 vec-w/len<=10)
            (λ (c) (c (vector 0)))
            (λ (_) (for/vector ([n (in-range 11)]) n))
            #:typed))

   ;; logical propositions about types
   (let ([pair-car-is-int (-refine/fresh p (-pair Univ Univ) (-is-type (-car-of  (-id-path p)) -Int))])
     (t pair-car-is-int)
     (t-int (-> pair-car-is-int pair-car-is-int)
            (λ (c) (c (cons 42 "any")))
            (λ (_) (cons 42 "any"))
            #:typed)
     (t-int/fail (-> pair-car-is-int pair-car-is-int)
                 (λ (c) (c (cons "any" "any")))
                 (λ (_) (cons 42 "any"))
                 #:typed
                 #:msg #rx"given: .*(\"any\" . \"any\")")
     (t-int/fail (-> pair-car-is-int pair-car-is-int)
                 (λ (c) (c (cons "any" "any")))
                 (λ (_) (cons "any" "any"))
                 #:untyped
                 #:msg #rx"produced: .*(\"any\" . \"any\")")
     ;; check range is not checked
     (t-int (-> pair-car-is-int pair-car-is-int)
            (λ (c) (c (cons 42 "any")))
            (λ (_) (cons "any" "any"))
            #:typed))
   (let ([pair-car-not-int (-refine/fresh p (-pair Univ Univ) (-not-type (-car-of (-id-path p)) -Int))])
     (t pair-car-not-int)
     (t-int (-> pair-car-not-int pair-car-not-int)
            (λ (c) (c (cons "any" 42)))
            (λ (_) (cons "any" 42))
            #:typed)
     (t-int/fail (-> pair-car-not-int pair-car-not-int)
                 (λ (c) (c (cons 42 "any")))
                 (λ (_) (cons "any" "any"))
                 #:typed
                 #:msg #rx"given: .*(42 . \"any\")")
     (t-int/fail (-> pair-car-not-int pair-car-not-int)
                 (λ (c) (c (cons "any" "any")))
                 (λ (_) (cons 42 "any"))
                 #:untyped
                 #:msg #rx"produced: .*(42 . \"any\")")
     ;; check range is not checked
     (t-int (-> pair-car-not-int pair-car-not-int)
            (λ (c) (c (cons "any" "any")))
            (λ (_) (cons 42 "any"))
            #:typed))

   (let ([int=42 (-refine/fresh x -Int (-and (-leq (-lexp x) (-lexp 42))
                                             (-leq (-lexp 42) (-lexp x))))])
     (t int=42)
     (t-int (-> int=42 int=42)
            (λ (c) (c 42))
            (λ (_) 42)
            #:typed)
     (t-int/fail (-> int=42 int=42)
                 (λ (c) (c "foo"))
                 (λ (_) 42)
                 #:typed
                 #:msg #rx"expected: exact-integer?.*given: .*\"foo\"")
     (t-int/fail (-> int=42 int=42)
                 (λ (c) (c 41))
                 (λ (_) 42)
                 #:typed
                 #:msg #rx"expected: .*given: .*41")
     (t-int/fail (-> int=42 int=42)
                 (λ (c) (c 43))
                 (λ (_) 42)
                 #:typed
                 #:msg #rx"expected: .*given: .*43")
     ;; range is skipped
     (t-int (-> int=42 int=42)
            (λ (c) (c 42))
            (λ (_) 41)
            #:typed))

   (let ([int<=0or>=100 (-refine/fresh x -Int (-or (-leq (-lexp x) (-lexp 0))
                                                   (-leq (-lexp 100) (-lexp x))))])
     (t int<=0or>=100)
     (t-int (-> int<=0or>=100 int<=0or>=100)
            (λ (c) (c -1))
            (λ (_) -1)
            #:typed)
     (t-int (-> int<=0or>=100 int<=0or>=100)
            (λ (c) (c 0))
            (λ (_) 0)
            #:typed)
     (t-int (-> int<=0or>=100 int<=0or>=100)
            (λ (c) (c 100))
            (λ (_) 100)
            #:typed)
     (t-int (-> int<=0or>=100 int<=0or>=100)
            (λ (c) (c 101))
            (λ (_) 101)
            #:typed)
     (t-int/fail (-> int<=0or>=100 int<=0or>=100)
                 (λ (c) (c "foo"))
                 (λ (_) -1)
                 #:typed
                 #:msg #rx"expected: exact-integer?.*given: .*\"foo\"")
     (t-int/fail (-> int<=0or>=100 int<=0or>=100)
                 (λ (c) (c 42))
                 (λ (_) -1)
                 #:typed
                 #:msg #rx"expected: .*given: .*42")
     ;; range is skipped
     (t-int (-> int<=0or>=100 int<=0or>=100)
            (λ (c) (c 120))
            (λ (_) 50)
            #:typed))
   (t/fail (-refine/fresh p (-pair Univ Univ) (-is-type (-car-of  (-id-path p)) (-vec Univ)))
           "proposition contract generation not supported for non-flat types")
   (t/fail (-refine/fresh p (-pair Univ Univ) (-not-type (-car-of  (-id-path p)) (-vec Univ)))
           "proposition contract generation not supported for non-flat types")

   ;; dependent functions // typed

   ;; identity on Integers
   (t-int (dep-> ([x : -Int])
                 (-refine/fresh n -Int (-eq (-lexp n) (-lexp x))))
          (λ (c) (c (random 100)))
          (λ (x) x)
          #:typed)
   ;; Int plus
   (t-int (dep-> ([x : -Int]
                  [y : -Int])
                 (-refine/fresh n -Int (-eq (-lexp n)
                                            (-lexp x y))))
          (λ (c) (c (random -100 100) (random -100 100)))
          (λ (x y) (+ x y))
          #:typed)
   ;; Nat Plus
   (t-int (dep-> ([x : -Nat]
                  [y : -Nat])
                 (-refine/fresh n -Nat (-eq (-lexp n)
                                            (-lexp x y))))
          (λ (c) (c (random 100) (random 100)))
          (λ (x y) (+ x y))
          #:typed)
   ;; tautilogical <=
   (t-int (dep-> ([x : -Nat]
                  [y : (-refine/fresh n -Nat (-leq (-lexp x)
                                                   (-lexp n)))])
                 -True)
          (λ (c) (c 0 0))
          (λ (x y) (<= x y))
          #:typed)
   (t-int (dep-> ([x : -Nat]
                  [y : (-refine/fresh n -Nat (-leq (-lexp x)
                                                   (-lexp n)))])
                 -True)
          (λ (c) (c 0 1))
          (λ (x y) (<= x y))
          #:typed)
   ;; safe vector-ref
   (t-int (-poly (a) (dep-> ([v : (-vec a)]
                             [n : (-refine/fresh n -Nat (-leq (-lexp n)
                                                              (-lexp (-vec-len-of (-id-path v)))))])
                            a))
          (λ (c) (c (vector 1 2) 0))
          (λ (v n) (vector-ref v n))
          #:typed)
   (t-int/fail (dep-> ([x : -Nat]
                       [y : (-refine/fresh n -Nat (-leq (-lexp x)
                                                        (-lexp n)))])
                      -True)
               (λ (c) (c 1 0))
               (λ (x y) #t)
               #:typed
               #:msg #rx"given:.*0.*and/c.*exact-nonnegative-integer?")
   (t-int/fail (-poly (a) (dep-> ([v : (-vec a)]
                                  [n : (-refine/fresh n -Nat (-leq (-lexp n)
                                                                   (-lexp (-vec-len-of (-id-path v)))))])
                                 a))
               (λ (c) (c (vector 1 2) -1))
               (λ (v n) (vector-ref v n))
               #:typed
               #:msg #rx"expected: natural?.*given:.*-1.*and/c")

   ;; dependent functions // untyped

   ;; identity on Integers
   (t-int (dep-> ([x : -Int])
                 (-refine/fresh n -Int (-eq (-lexp n) (-lexp x))))
          (λ (c) (c (random 100)))
          (λ (x) x)
          #:untyped)
   ;; Int plus
   (t-int (dep-> ([x : -Int]
                  [y : -Int])
                 (-refine/fresh n -Int (-eq (-lexp n)
                                            (-lexp x y))))
          (λ (c) (c (random -100 100) (random -100 100)))
          (λ (x y) (+ x y))
          #:untyped)
   ;; Nat Plus
   (t-int (dep-> ([x : -Nat]
                  [y : -Nat])
                 (-refine/fresh n -Nat (-eq (-lexp n)
                                            (-lexp x y))))
          (λ (c) (c (random 100) (random 100)))
          (λ (x y) (+ x y))
          #:untyped)
   ;; tautilogical <=
   (t-int (dep-> ([x : -Nat]
                  [y : (-refine/fresh n -Nat (-leq (-lexp x)
                                                   (-lexp n)))])
                 -True)
          (λ (c) (c 0 0))
          (λ (x y) (<= x y))
          #:untyped)
   (t-int (dep-> ([x : -Nat]
                  [y : (-refine/fresh n -Nat (-leq (-lexp x)
                                                   (-lexp n)))])
                 -True)
          (λ (c) (c 0 1))
          (λ (x y) (<= x y))
          #:untyped)
   ;; safe vector-ref
   (t-int (-poly (a) (dep-> ([v : (-vec a)]
                             [n : (-refine/fresh n -Nat (-leq (-lexp n)
                                                              (-lexp (-vec-len-of (-id-path v)))))])
                            -True))
          (λ (c) (c (vector 1 2) 0))
          (λ (v n) #t)
          #:untyped)
   (t-int/fail (dep-> ([x : -Nat]
                       [y : (-refine/fresh n -Nat (-leq (-lexp x)
                                                        (-lexp n)))])
                      -True)
               (λ (c) (c 0 1))
               (λ (x y) #f)
               #:untyped
               #:msg #rx"promised:.*#t.*produced:.*#f")

   ;; #:rest-star args
   (t (->* (list -Zero) (make-Rest (list -Zero -Symbol)) -Boolean))
   (t (->* (list -Zero) (make-Rest (list -Zero -Zero)) -Zero))
   (t (->* (list) (make-Rest (list -Boolean -String -Boolean -String -Boolean -String)) -Zero))
   (t (->optkey [-Zero] #:rest Univ -Boolean))
   (t-int (->* (list -Zero) (make-Rest (list -Zero -Symbol)) -Boolean)
          (λ (c) (c 0))
          (case-lambda
            [(zero) #t]
            [(zero . rst) #f])
          #:untyped)
   (t-int (->* (list -Zero) (make-Rest (list -Zero -Symbol)) -Boolean)
          (λ (c) (c 0))
          (case-lambda
            [(zero) #t]
            [(zero . rst) #f])
          #:typed)
   (t-int (->* (list -Zero) (make-Rest (list -Zero -Symbol)) -Boolean)
          (λ (c) (c 0 0 'zero))
          (case-lambda
            [(zero) #t]
            [(zero . rst) #f])
          #:untyped)
   (t-int (->* (list -Zero) (make-Rest (list -Zero -Symbol)) -Boolean)
          (λ (c) (c 0 0 'zero 0 'zero))
          (case-lambda
            [(zero) #t]
            [(zero . rst) #f])
          #:untyped)
   (t-int (->* (list -Zero) (make-Rest (list -Zero -Symbol)) -Boolean)
          (λ (c) (c 0 0 'zero 0 'zero))
          (case-lambda
            [(zero) #t]
            [(zero . rst) #f])
          #:typed)
   ;; shouldn't error since we should trust the typed side, right?
   ;(t-int/fail (->* (list -Zero) (make-Rest (list -Zero -Symbol)) -Boolean)
   ;       (λ (c) (c 'zero 'zero))
   ;       (case-lambda
   ;         [(zero) #t]
   ;         [(zero . rst) #f])
   ;       #:untyped
   ;       #:msg #rx"given: '(zero)")
   (t-int/fail (->* (list -Zero) (make-Rest (list -Zero -Symbol)) -Boolean)
               (λ (c) (c 0))
               (case-lambda
                 [(zero) 'true]
                 [(zero . rst) 'false])
               #:untyped
               #:msg #rx"produced: 'true")
   (t-int/fail (->* (list -Zero) (make-Rest (list -Zero -Symbol)) -Boolean)
               (λ (c) (c 0))
               (case-lambda
                 [(zero) 'true]
                 [(zero . rst) 'false])
               #:untyped
               #:msg #rx"produced: 'true")
   (t-int/fail (->* (list -Zero) (make-Rest (list -Zero -Symbol)) -Boolean)
          (λ (c) (c 0 'zero))
          (case-lambda
            [(zero) #t]
            [(zero . rst) #f])
          #:typed
          #:msg #rx"contract violation")
   (t-int/fail (->* (list -Zero) (make-Rest (list -Zero -Symbol)) -Boolean)
          (λ (c) (c 0 0 0))
          (case-lambda
            [(zero) #t]
            [(zero . rst) #f])
          #:typed
          #:msg #rx"contract violation")
   (t-int/fail (->* (list -Zero) (make-Rest (list -Zero -Symbol)) -Boolean)
          (λ (c) (c 0 0 'zero 0))
          (case-lambda
            [(zero) #t]
            [(zero . rst) #f])
          #:typed
          #:msg #rx"contract violation")
   ;; prefab contracts
   (t (-prefab 'point -Zero -Zero))
   (t (-prefab 'point (-prefab 'point -Zero -Zero) (-prefab 'point -Zero -Zero)))
   (t (-prefab 'fun (-Number . -> . -Number)))
   (t (-prefab '(box-box #(0)) (-box -Number)))
   (t (-prefab-top 'point 2))
   (t (-prefab-top '(box-box #(0)) 1))
   (t-int (-val #rx"aa") void #rx"aa" #:untyped)
   (t-int (-val #rx#"bb") void #rx#"bb" #:untyped)

   (t-int/fail -Async-ChannelTop async-channel-get (let ([ch (make-async-channel)]) (async-channel-put ch "ok") ch)
          #:typed
          #:msg "Attempted to use a higher-order value passed as `Any`")
   (t-int -Async-ChannelTop async-channel-get (let ([ch (make-async-channel)]) (async-channel-put ch "ok") ch)
          #:untyped)
   (t-int/fail -MPairTop mcar (mcons 0 0)
          #:typed
          #:msg "Attempted to use a higher-order value passed as `Any`")
   (t-int -MPairTop mcar (mcons 0 0)
          #:untyped)
   (t-int/fail -HashTableTop (lambda (h) (hash-set! h 'a 0)) (make-hash `((a . b)))
               #:typed
               #:msg "Attempted to use a higher-order value passed as `Any`")
   (t-int -HashTableTop (lambda (h) (hash-set! h 'a 0)) (make-hash `((a . b)))
          #:untyped)
   (t-int/fail -Mutable-HashTableTop (lambda (h) (hash-set! h 'a 0)) (make-hash `((a . b)))
               #:typed
               #:msg "Attempted to use a higher-order value passed as `Any`")
   (t-int -Mutable-HashTableTop (lambda (h) (hash-set! h 'a 0)) (make-hash `((a . b)))
          #:untyped)
   (t-int/fail -Weak-HashTableTop (lambda (h) (hash-set! h 'a 0)) (make-weak-hash `((a . b)))
               #:typed
               #:msg "Attempted to use a higher-order value passed as `Any`")
   (t-int -Weak-HashTableTop (lambda (h) (hash-set! h 'a 0)) (make-weak-hash `((a . b)))
          #:untyped)
   (t-int/fail -ThreadCellTop (lambda (tc) (thread-cell-set! tc 42)) (make-thread-cell 'x)
               #:typed
               #:msg "Attempted to use a higher-order value passed as `Any`")
   (t-int -ThreadCellTop (lambda (tc) (thread-cell-set! tc 42)) (make-thread-cell 'x)
          #:untyped)
   (t-int/fail -Prompt-TagTop continuation-prompt-available? (make-continuation-prompt-tag)
               #:typed
               #:msg "Attempted to use a higher-order value passed as `Any`")
   (t-int -Prompt-TagTop continuation-prompt-available? (make-continuation-prompt-tag)
          #:untyped)
   (t-int/fail -Continuation-Mark-KeyTop (lambda (k) (continuation-mark-set-first #f k)) (make-continuation-mark-key)
               #:typed
               #:msg "Attempted to use a higher-order value passed as `Any`")
   (t-int -Continuation-Mark-KeyTop (lambda (k) (continuation-mark-set-first #f k)) (make-continuation-mark-key)
          #:untyped)
   (t-int/fail -ClassTop class->interface object%
               #:typed
               #:msg "Attempted to use a higher-order value passed as `Any`")
   (t-int -ClassTop class->interface object%
          #:untyped)
   (t/fail (make-Ephemeron -Symbol)
           "contract generation not supported for this type")
   (t/fail (make-Future -Symbol)
           "contract generation not supported for this type")
   (t-int -ChannelTop
          channel-get
          (let ((ch (make-channel))) (thread (λ () (channel-put ch "ok"))) ch)
          #:typed)
   (t-int -ChannelTop
          channel-get
          (let ((ch (make-channel))) (thread (λ () (channel-put ch "ok"))) ch)
          #:untyped)
   (t-int/fail -ChannelTop
               (lambda (ch)
                 (channel-put ch 'error))
               (make-channel)
               #:typed
               #:msg "higher-order value passed as `Any`")
   (t-sc top-func (case->/sc '()))
   (t-int -NonNegInexactReal void 2.0 #:untyped)
  )

  (test-suite
   "Shallow Tests"
   (t-sc ((-poly (a) (-vec a)) . -> . -Symbol)
         (make-procedure-arity-flat/sc 1 '() '()) #:shallow)
   (t-sc -Number number/sc #:shallow)
   (t-int Any-Syntax syntax? #'#'A #:typed #:shallow)
   (t-int (-poly (a) (-> a a))
          (λ (f) (f 1))
          (λ (x) 1)
          #:untyped #:shallow)

   (t (-Number . -> . -Number) #:shallow)

   (t-sc (make-Ephemeron -Symbol) ephemeron?/sc #:shallow)
   (t-sc (make-Future -Symbol) future?/sc #:shallow)
   (t-sc (-mpair -Symbol -Symbol) mpair?/sc #:shallow)

   (t-sc -MPairTop mpair?/sc #:shallow)
   (t-sc -BoxTop box?/sc #:shallow)
   (t-sc -HashTableTop hash?/sc #:shallow)
   (t-sc -Mutable-VectorTop mutable-vector?/sc #:shallow)
   (t-sc -VectorTop vector?/sc #:shallow)
   (t-sc -ChannelTop channel?/sc #:shallow)
   (t-sc -Async-ChannelTop async-channel?/sc #:shallow)
   (t-sc -ThreadCellTop thread-cell?/sc #:shallow)
   (t-sc -Weak-BoxTop weak-box?/sc #:shallow)
   (t-sc -Mutable-HashTableTop mutable-hash?/sc #:shallow)
   (t-sc -Weak-HashTableTop weak-hash?/sc #:shallow)
   (t-sc -Prompt-TagTop prompt-tag?/sc #:shallow)
   (t-sc -Continuation-Mark-KeyTop continuation-mark-key?/sc #:shallow)
   (t-sc -StructTypeTop struct-type?/sc #:shallow)
   (t-sc -ClassTop class?/sc #:shallow)
   (t-sc -UnitTop unit?/sc #:shallow)
   (t-sc -SequenceTop sequence?/sc #:shallow)

   (t-sc (-val eof) (flat/sc #'eof-object?) #:shallow)
   (t-sc (-val (void)) (flat/sc #'void?) #:shallow)
   (t-sc (-val 'X) (flat/sc #'(lambda (x) (eq? x 'X))) #:shallow)
   (t-sc (-val #t) (flat/sc #'(lambda (x) (eq? x '#t))) #:shallow)
   (t-sc (-val '#:kw) (flat/sc #'(lambda (x) (eq? x '#:kw))) #:shallow)
   (t-sc (-val '()) (flat/sc #'(lambda (x) (eq? x '()))) #:shallow)
   (t-sc (-val 3+3i) (flat/sc #'(lambda (x) (equal? x '3+3i))) #:shallow)
   (t-sc (-val #rx"aa") (flat/sc #'(lambda (x) (equal? x '#rx"aa"))) #:shallow)
   (t-sc (-val #rx#"bb") (flat/sc #'(lambda (x) (equal? x '#rx#"bb"))) #:shallow)
   (t-sc (-val "cc") (flat/sc #'(lambda (x) (equal? x '"cc"))) #:shallow)
   (t-sc (-val #"dd") (flat/sc #'(lambda (x) (equal? x '#"dd"))) #:shallow)
   (t-sc (-val #\e) (flat/sc #'(lambda (x) (equal? x '#\e))) #:shallow)

   (t-sc (-ivec* -Symbol) (immutable-vector-length/sc 1) #:shallow)
   (t-sc (-mvec* -Symbol -Symbol -Symbol) (mutable-vector-length/sc 3) #:shallow)
   (t-sc (-lst* -Symbol -Symbol) (list-length/sc 2) #:shallow)

   (t -Byte-Regexp #:shallow)

   (t-sc (-polydots (a) (-> (->... (list) (a a) -Symbol) (->... (list) (a a) -Symbol)))
         (make-procedure-arity-flat/sc 1 '() '()) #:shallow)
   (t-sc (-polydots (a) (->... (list) (a a) -Symbol))
         procedure?/sc #:shallow)
  (t-sc
    (-polydots (a b)
               (->... (list) ((-lst* (->... (list) (a a) b)) b) Univ))
    procedure?/sc
    #:shallow)
  (t-sc (-polydots (a) (-lst* (->... (list) (a a) Univ)))
        (list-length/sc 1) #:shallow)
  (t-sc (make-ListDots Univ 'x)
        list?/sc #:shallow)
  (t-sc (make-SequenceDots (list) Univ 'x)
        sequence?/sc #:shallow)
  (t-int/fail (make-Value 3.0)
              values
              3
              #:untyped #:shallow
              #:msg #rx"produced: 3")
  (t-int/fail (make-Value 3)
              values
              3.0
              #:untyped #:shallow
              #:msg #rx"produced: 3.0")
  (t-sc top-func procedure?/sc #:shallow)
  (t (-polyrow (a) (list null null null null)
               (-> (-class #:row (-v a)) (-class #:row (-v a))))
     #:shallow)
  (t (-polyrow (a) (list null null null null)
               (-> (-class #:method ([m (-> -Symbol -Integer)])) (-class #:method ([m (-> -Symbol -Integer)]))))
     #:shallow)
  (t-sc (make-StructTop (make-Struct #'foo #f null #f #f #'foo? (immutable-free-id-set (list))))
        (flat/sc #'(lambda (x) (foo? x)))
        #:shallow)
  (t-sc (-prefab-top 'point 2)
        (flat/sc #'(struct-type-make-predicate
                    (prefab-key->struct-type 'point 2)))
        #:shallow)
  (t-sc (-prefab-top '(box-box #(0)) 1)
        (flat/sc #'(struct-type-make-predicate
                    (prefab-key->struct-type (quote (box-box #(0))) 1)))
        #:shallow)

  )
))
