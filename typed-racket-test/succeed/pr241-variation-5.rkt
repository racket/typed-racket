#lang racket/base

;; Check that any-wrap/c handles all base types
;; We do this by:
;; - enumerating the base types from `base-env/base-types.rkt`
;; - for each type:
;;   - define a value
;;   - typecheck the value
;;   - wrap the value as 'Any', check failure / use in untyped code

(require
  (for-syntax racket/base)
  racket/require)

;; When #t, print a warning if some base-types do not have tests
(define-for-syntax WARN-MISSING #t)

;; `known-base-types` is a list of triples:
;;  [TYPE VALUE USE]
;; TYPE is a base type exported by Typed Racket
;; VALUE is an expression with type TYPE
;; USE is an untyped function of 1 argument, or #f.
;;  If a function, a test will apply (USE (contract VALUE any-wrap/c)).
;;  If #f, a test will assert that (contract VALUE any-wrap/c) fails.
;; These USE functions should exercise specific functionality
;;  and not be dummy functions like (lambda (x) x).
(define-for-syntax known-base-types '(
 ;; --- TODO missing value for these types
 ;[Read-Table (or (current-readtable) (error 'noo)) #f]
 ;[Internal-Definition-Context (syntax-local-make-definition-context) #f]
 ;[Place (place hi (void)) place-kill]

 ;; --- Types that should NOT be passes as 'Any' are marked with #f
 [Async-ChannelTop (make-async-channel) #f]
 [ClassTop object% #f]
 [Compiled-Expression (compile-syntax #'#t) #f]
 [Continuation-Mark-KeyTop (make-continuation-mark-key) #f]
 [Continuation-Mark-Set (current-continuation-marks) #f]
 [Custodian (current-custodian) #f]
 [Identifier (syntax exit) #f]
 [MPairTop (mcons 1 1) #f]
 [Namespace (make-empty-namespace) #f]
 [Parameterization (current-parameterization) #f]
 [Prompt-TagTop (make-continuation-prompt-tag) #f]
 [Security-Guard (current-security-guard) #f]
 [Special-Comment (make-special-comment 'hi) #f]
 [Struct-Type-Property (let-values ([(n g s) (make-struct-type-property 'foo)]) n) #f]
 [Syntax (syntax 'B) #f]
 [Syntax-E (syntax-e (syntax 'A)) #f]
 [Thread-CellTop (make-thread-cell 'X) #f]
 [UnitTop (unit (import) (export)) #f]
 [Variable-Reference (let ([x 4]) (#%variable-reference x)) #f]
 [Weak-BoxTop (make-weak-box 3) #f]

 ;; -- Normal base types
 [Subprocess
  (let*-values ([(sp _out _in _err) (subprocess #f #f #f ".")])
    (close-output-port _in)
    (close-input-port _out)
    (close-input-port _err)
    sp)
  choice-evt]
 [Single-Flonum-Complex 1f0+1f0i add1]
 [ExtFlonum-Zero 0.0t0 extflround]
 [ExtFlonum-Negative-Zero -0.0t0 extflround]
 [ExtFlonum-Positive-Zero +0.0t0 extflround]
 [Complex 0 add1]
 [Number 0 add1]
 [Inexact-Complex (let ([n (exact->inexact 1/3+1/3i)]) (if (not (real? n)) n (error 'pr241 "Failed to make Inexact-Complex"))) zero?]
 [Float-Complex 1.0+1i add1]
 [Exact-Number 0 add1]
 [Real 0 add1]
 [Nonpositive-Real 0 add1]
 [Negative-Real -1 add1]
 [Nonnegative-Real 0 add1]
 [Positive-Real 1 add1]
 [Real-Zero 0 add1]
 [Inexact-Real (exact->inexact 1/3) add1]
 [Single-Flonum 1.0f0 add1]
 [Nonpositive-Inexact-Real (- (exact->inexact 1/3)) add1]
 [Nonpositive-Single-Flonum -1.0f0 add1]
 [Negative-Inexact-Real -1.0f0 add1]
 [Negative-Single-Flonum -1.0f0 add1]
 [Positive-Single-Flonum +1.0f0 add1]
 [Nonnegative-Inexact-Real (exact->inexact 1/3) add1]
 [Nonnegative-Single-Flonum 1.0f0 add1]
 [Positive-Inexact-Real 1.0f0 add1]
 [Inexact-Real-Nan +nan.0 zero?]
 [Inexact-Real-Zero 0.0 add1]
 [Inexact-Real-Negative-Zero -0.0 add1]
 [Inexact-Real-Positive-Zero 0.0 add1]
 [Single-Flonum-Nan +nan.f add1]
 [Single-Flonum-Zero 0f0 add1]
 [Single-Flonum-Negative-Zero -0f0 add1]
 [Single-Flonum-Positive-Zero 0f0 add1]
 [Float 1.0 add1]
 [Flonum 1.0 add1]
 [Nonpositive-Float -1.0 add1]
 [Nonpositive-Flonum -1.0 add1]
 [Negative-Float -1.0 add1]
 [Negative-Flonum -1.0 add1]
 [Nonnegative-Float 1.0 add1]
 [Nonnegative-Flonum 1.0 add1]
 [Positive-Float 1.0 add1]
 [Positive-Flonum 1.0 add1]
 [Float-Nan +nan.0 add1]
 [Flonum-Nan +nan.0 add1]
 [Float-Zero 0.0 add1]
 [Flonum-Zero 0.0 add1]
 [Float-Negative-Zero -0.0 add1]
 [Flonum-Negative-Zero -0.0 add1]
 [Float-Positive-Zero 0.0 add1]
 [Flonum-Positive-Zero 0.0 add1]
 [Exact-Rational 1/3 add1]
 [Nonpositive-Exact-Rational 0/1 add1]
 [Negative-Exact-Rational -3/2 add1]
 [Nonnegative-Exact-Rational 1 add1]
 [Positive-Exact-Rational 4/3 add1]
 [Integer 0 add1]
 [Nonpositive-Integer 0 add1]
 [Negative-Integer -2 add1]
 [Exact-Nonnegative-Integer 0 add1]
 [Nonnegative-Integer 0 add1]
 [Natural 62 add1]
 [Exact-Positive-Integer 6 add1]
 [Positive-Integer 6 add1]
 [Fixnum 9 add1]
 [Negative-Fixnum -3 add1]
 [Nonpositive-Fixnum -4 add1]
 [Nonnegative-Fixnum 4 add1]
 [Positive-Fixnum 2 add1]
 [Index 3 add1]
 [Positive-Index 3 add1]
 [Byte 0 add1]
 [Positive-Byte 1 add1]
 [Zero 0 add1]
 [One  1 add1]
 [ExtFlonum pi.t extflround]
 [Nonpositive-ExtFlonum (->extfl -1) extflround]
 [Negative-ExtFlonum (->extfl -1) extflround]
 [Nonnegative-ExtFlonum (->extfl 1) extflround]
 [Positive-ExtFlonum (->extfl 1) extflround]
 [ExtFlonum-Nan +nan.t (lambda (n) (extfl= n n))]

 [Any 'a boolean?]
 [Boolean #f not]
 [BoxTop (box 5) boolean?]
 [Byte-PRegexp (byte-pregexp #"\\d\\d") (lambda (p) (regexp-match? p "013a"))]
 [Byte-Regexp (byte-regexp #"hi$") (lambda (p) (regexp-match? p "hi"))]
 [Bytes #"hello" bytes-length]
 [Bytes-Converter (or (bytes-open-converter "UTF-8" "UTF-8") (error 'pr241 "Failed to make bytes converter")) bytes-close-converter]
 [ChannelTop (make-channel) channel-try-get]
 [Char #\space char->integer]
 [Datum 'A (lambda (x) (datum->syntax #f x))]
 [Environment-Variables (current-environment-variables) environment-variables-names]
 [EOF eof eof-object?]
 [ExtFlVector (extflvector pi.t) extflvector-length]
 [FSemaphore (make-fsemaphore 0) fsemaphore-post]
 [False #f not]
 [FlVector (flvector 1.14 2.14 3.14) flvector-length]
 [FxVector (fxvector 1) fxvector-length]
 [HashTableTop (hash) (lambda (h) (hash-ref h 'a #f))]
 [Impersonator-Property (let-values ([(i i? i-val) (make-impersonator-property 'i)]) i) (lambda (i) (impersonate-procedure (lambda () (void)) #f i 2))]
 [Input-Port (current-input-port) port?]
 [Inspector (current-inspector) (lambda (i) (parameterize ([current-inspector i]) (void)))]
 [Keyword (string->keyword "hi") keyword->string]
 [Log-Level 'info symbol->string]
 [Log-Receiver (make-log-receiver (current-logger) 'info) choice-evt]
 [Logger (current-logger) (lambda (l) (log-level? l 'info))]
 [Module-Path "hello.rkt" module-path?]
 [Mutable-HashTableTop (make-hash) (lambda (h) (hash-ref h 'a #f))]
 [Mutable-VectorTop (vector 1 2 3) (lambda (x) (vector-ref x 0))]
 [Null '() length]
 [Output-Port (current-output-port) port?]
 [PRegexp #px"\\d\\d" (lambda (p) (regexp-match? p "013a"))]
 [Path (current-directory) path->string]
 [Path-For-Some-System (current-directory) path->string]
 [Path-String "foo/bar" relative-path?]
 [Place-Channel (let-values ([(p1 p2) (place-channel)]) p1) choice-evt]
 [Port (current-input-port) port?]
 [Pretty-Print-Style-Table (pretty-print-current-style-table) (lambda (x) (pretty-print-extend-style-table x '() '()))]
 [Procedure (lambda (x) x) (lambda (f) (procedure-arity-includes? f 1))]
 [Pseudo-Random-Generator (current-pseudo-random-generator) pseudo-random-generator->vector]
 [Regexp #rx"hi$" (lambda (p) (regexp-match? p "hi"))]
 [Resolved-Module-Path (make-resolved-module-path (current-directory)) resolved-module-path-name]
 [Semaphore (make-semaphore) semaphore-post]
 [Sexp (syntax->datum (syntax 'foo)) (lambda (x) x)]
 [String "yolo" string->symbol]
 [Symbol 'a symbol->string]
 [TCP-Listener (tcp-listen 0) choice-evt]
 [Thread (thread (lambda () (void))) choice-evt]
 [Thread-Group (current-thread-group) make-thread-group]
 [True #t not]
 [UDP-Socket (udp-open-socket) udp-close]
 [VectorTop (vector 1 2 3) (lambda (x) (vector-ref x 0))]
 [Void (void) void?]
 [Weak-HashTableTop (make-weak-hash) (lambda (h) (hash-ref h 'a #f))]
 [Will-Executor (make-will-executor) choice-evt]
))

(define-values-for-syntax (base-untyped* base-typed*)
  (for/fold ([u* '()] [t* '()])
            ([tvf (in-list known-base-types)])
    (unless (and (list? tvf) (= 3 (length tvf)))
      (error 'pr241 "Expected (TYPE VAL FUN)"))
    (with-syntax ([type (car tvf)]
                  [val (cadr tvf)]
                  [use (caddr tvf)]
                  [pos-blame 'pr241-test]
                  [neg-blame 'known-base-types]
                  [id (gensym (car tvf))])
      ;; Wrap value in contract, pass it to `use` or assert a wrap failure
      ;; Ignore the result
      (define u-stx
        (if (eq? #f (caddr tvf))
          #'(with-handlers ([exn:fail:contract? (lambda (e) (void))])
              (contract any-wrap/c val 'pos-blame 'neg-blame)
              (error 'pr241 (format "Higher-Order value '~a' incorrectly allowed as Any" val)))
          #'((lambda any* (void))
             (use (contract any-wrap/c val 'pos-blame 'neg-blame)))))
      ;; Bind the value to a typed identifier
      (define t-stx
        #'(begin (: id type) (define id val)))
      (values
        (cons u-stx u*)
        (cons t-stx t*)))))

;; `known-polymorphic-types` is a list of 4-lists:
;;   [TYPE-TAG TYPE VALUE USE]
;; TYPE-TAG is a polymorphic type exported by Typed Racket
;; TYPE is an instantiation of TYPE-TAG
;; VALUE is an expression with type TYPE
;; USE is the same as for `known-base-types`
(define-for-syntax known-poly-types '(
  ;; --- Higher-Order polymorphic types
  [Async-Channelof (Async-Channelof Any) (make-async-channel) #f]
  [Continuation-Mark-Keyof (Continuation-Mark-Keyof Any) (make-continuation-mark-key 'X) #f]
  [Custodian-Boxof (Custodian-Boxof Integer) (make-custodian-box (current-custodian) 1) #f]
  [Ephemeronof (Ephemeronof Integer) (make-ephemeron 'key 4) #f]
  [Evtof (Evtof String) never-evt choice-evt]
  [Futureof (Futureof Integer) (future (lambda () 4)) #f]
  [MPairof (MPairof Integer String) (mcons 4 "ad") #f]
  [MListof (MListof Integer) (mcons 4 '()) #f]
  [Prompt-Tagof (Prompt-Tagof Any Any) (make-continuation-prompt-tag) #f]
  [Syntaxof (Syntaxof Integer) #'1 #f]
  [Thread-Cellof (Thread-Cellof Integer) (make-thread-cell 1) #f]
  [Weak-Boxof (Weak-Boxof Integer) (make-weak-box 1) #f]

  ;; -- Wrappable polymorphic types
  [Boxof (Boxof Integer) (box 3) (lambda (v) (add1 (unbox v)))]
  [Channelof (Channelof Integer) (make-channel) channel-try-get]
  [HashTable (HashTable Symbol String) (hash) (lambda (h) (hash-ref h 'a #f))]
  [Immutable-HashTable (Immutable-HashTable Symbol String) (hash) (lambda (h) (hash-set h 'a "a"))]
  [Immutable-Vector (Immutable-Vector Integer) (vector-immutable 1) (lambda (v) (vector-ref v 0))]
  [Immutable-Vectorof (Immutable-Vectorof Integer) (vector-immutable 1) (lambda (v) (vector-ref v 0))]
  [Listof (Listof Integer) (list 1) (lambda (xs) (add1 (car xs)))]
  [Mutable-HashTable (Mutable-HashTable Symbol String) (make-hash)
   (lambda (h)
     (hash-ref h 'a #f)
     (with-handlers ([exn:fail:contract? void])
       (hash-set! h 'a "a")
       (error 'pr241 "mutable hashtable ~a incorrectly allowed to be set!" h)))]
  [Mutable-Vector (Mutable-Vector Integer) (vector 1)
   (lambda (v)
     (vector-ref v 0)
     (with-handlers ([exn:fail:contract? void])
       (vector-set! v 0 1)
       (error 'pr241 "mutable vector ~a incorrectly allowed to be set!" v)))]
  [Mutable-Vectorof (Mutable-Vectorof Integer) (vector 1)
   (lambda (v)
     (vector-ref v 0)
     (with-handlers ([exn:fail:contract? void])
       (vector-set! v 0 1)
       (error 'pr241 "mutable vectorof ~a incorrectly allowed to be set!" v)))]
  [Option (Option Integer) 1 add1]
  [Pair (Pair Integer Boolean) (cons 1 #t) (lambda (v) (add1 (car v)))]
  [Pairof (Pairof Integer Boolean) (cons 1 #f) (lambda (v) (add1 (car v)))]
  [Promise (Promise Integer) (delay 3) force]
  [Sequenceof (Sequenceof Natural) '(1 2 3) sequence->list]
  [Setof (Setof Integer) (set) set-empty?]
  [Sexpof (Sexpof Integer) (syntax->datum #'(1 2 3)) (lambda (xs) (add1 (car xs)))]
  [Weak-HashTable (Weak-HashTable Symbol String) (make-weak-hash)
   (lambda (h)
     (hash-ref h 'a #f)
     (with-handlers ([exn:fail:contract? void])
       (hash-set! h 'a "a")
       (error 'pr241 "weak hashtable ~a incorrectly allowed to be set!" h)))]
  [Vector (Vector Integer) (vector 1) (lambda (v) (vector-ref v 0))]
  [Vectorof (Vectorof Integer) (vector 1) (lambda (v) (add1 (vector-ref v 0)))]
))

;; We don't have values to test these types with, but trust they're wrapped correctly
(define-for-syntax whitelist '(
 (Undefined . #f)
 (Nothing . #f)
 (Struct-TypeTop . #f)
 (Module-Path-Index . #f)
 (Compiled-Module-Expression . #f)
 (Namespace-Anchor . #f)
))

(define-values-for-syntax (poly-untyped* poly-typed*)
  (for/fold ([u* '()] [t* '()])
            ([gtvf (in-list known-poly-types)])
    (unless (and (list? gtvf) (= 4 (length gtvf)))
      (error 'pr241 "Expected (TYPE-TAG TYPE VAL FUN)"))
    (with-syntax ([_tag (car gtvf)] ;; Unused here
                  [type (cadr gtvf)]
                  [val (caddr gtvf)]
                  [use (cadddr gtvf)]
                  [pos-blame 'pr241-test]
                  [neg-blame 'known-poly-types]
                  [id (gensym (car gtvf))])
      ;; Wrap value in contract, pass it to `use` or assert a wrap failure
      ;; Ignore the result
      (define u-stx
        (if (eq? #f (cadddr gtvf))
          #'(with-handlers ([exn:fail:contract? (lambda (e) (void))])
              (contract any-wrap/c val 'pos-blame 'neg-blame)
              (error 'pr241 (format "Higher-Order value '~a' incorrectly allowed as Any" val)))
          #'((lambda any* (void))
             (use (contract any-wrap/c val 'pos-blame 'neg-blame)))))
      ;; Bind the value to a typed identifier
      (define t-stx
        #'(begin (: id type) (define id val)))
      (values
        (cons u-stx u*)
        (cons t-stx t*)))))

(define-syntax (known-types->tests stx)
  ;; Put the `use-` list in an untyped module,
  ;;  put the `declare-` list in a typed module.
  #`(begin
      (module check-uses racket
       (require
         racket/async-channel
         racket/flonum
         racket/extflonum
         racket/fixnum
         typed-racket/utils/any-wrap)
       #,@base-untyped*
       #,@poly-untyped*
       (printf "Successfully used ~a wrapped values\n"
         #,(+ (length base-untyped*) (length poly-untyped*))))
      (module check-types typed/racket
        (require
          racket/flonum
          racket/fixnum
          racket/extflonum)
        (require/typed racket/base
          [variable-reference->module-path-index (-> Any Module-Path-Index)])
        (require/typed racket/async-channel
          [make-async-channel (-> (Async-Channelof Any))])
        #,@base-typed*
        #,@poly-typed*
        (printf "Successfully typechecked ~a identifiers\n"
          #,(+ (length base-typed*) (length poly-typed*))))))

(define-for-syntax (known-string? str)
  (define s (string->symbol str))
  (member* s known-base-types known-poly-types whitelist))

(define-for-syntax (member* s . x**)
  (for*/or ([x* (in-list x**)]
            [x (in-list x*)])
    (eq? s (car x))))

(require
  (filtered-in
    (if WARN-MISSING
      (lambda (str)
        (when (not (known-string? str)) (printf "WARNING: Missing test for base type '~a'\n" str))
        #f)
      (lambda (str) #f))
    typed-racket/base-env/base-types))

(known-types->tests)

(require
 'check-uses
 'check-types)
