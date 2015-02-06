#lang typed/racket
(require typed/racket/class
         typed/private/utils
         typed/private/rewriter
         "type-env-ext.rkt"
         (for-syntax syntax/parse))

(define-type check-ish-ty
  (case-lambda
    (Any Any -> Any)
    (Any Any String -> Any)))
(define-type (Predicate A) (A -> Boolean))
(define-type (Thunk A) (-> A))

; 3.2
(require/typed/provide
 rackunit
 [check-eq? check-ish-ty]
 [check-not-eq? check-ish-ty]
 [check-eqv? check-ish-ty]
 [check-not-eqv? check-ish-ty]
 [check-equal? check-ish-ty]
 [check-not-equal? check-ish-ty]
 [check-pred
  (All (A)
       (case-lambda
         ((A -> Any) A -> Any)
         ((A -> Any) A String -> Any)))]
 [check-=
  (case-lambda
    (Real Real Real -> Any)
    (Real Real Real String -> Any))]
 [check-true
  (case-lambda
    (Any -> Any)
    (Any String -> Any))]
 [check-false
  (case-lambda
    (Any -> Any)
    (Any String -> Any))]
 [check-not-false
  (case-lambda
    (Any -> Any)
    (Any String -> Any))]
 [check-exn
  (case-lambda
    ((U (Predicate Any) Regexp) (Thunk Any) -> Any)
    ((U (Predicate Any) Regexp) (Thunk Any) String -> Any))]
 [check-not-exn
  (case-lambda
    ((Thunk Any) -> Any)
    ((Thunk Any) String -> Any))]
 [check-regexp-match
  (Regexp String -> Any)]


 [check (All (A B)
             (case-lambda
               ((A B -> Any) A B -> Any)
               ((A B -> Any) A B String -> Any)))]

 [fail
  (case-lambda
    (-> Void)
    (String -> Void))])

; 3.2.1
(require-typed-struct check-info
                      ([name : Symbol] [value : Any])
                      rackunit)
(define-type CheckInfo check-info)
(provide (struct-out check-info) CheckInfo)
(require/typed/provide
 rackunit
 [make-check-name (String -> CheckInfo)]
 [make-check-params ((Listof Any) -> CheckInfo)]
 [make-check-location ((List Any (Option Number) (Option Number) (Option Number) (Option Number)) -> CheckInfo)]
 [make-check-expression (Any -> CheckInfo)]
 [make-check-message (String -> CheckInfo)]
 [make-check-actual (Any -> CheckInfo)]
 [make-check-expected (Any -> CheckInfo)]
 [with-check-info* (All (A) ((Listof CheckInfo) (Thunk A) -> A))])
(require (only-in rackunit with-check-info))
(provide with-check-info)

; 3.2.2
(require (only-in rackunit define-simple-check define-binary-check define-check fail-check))
(provide define-simple-check define-binary-check define-check fail-check)

; 3.3
(require (prefix-in t: (except-in rackunit struct:check-info struct:exn:test struct:exn:test:check struct:test-result struct:test-failure
                                  struct:test-error struct:test-success)))
(define-rewriter t:test-begin test-begin
  [t:current-test-case-around current-test-case-around]
  [t:check-around check-around]
  [t:current-check-handler current-check-handler]
  [t:current-check-around current-check-around])

(define-syntax-rule (test-case name expr ...)
  (parameterize
      ([current-test-name (ann name String)])
    (test-begin expr ...)))

(provide test-begin test-case)

(require/opaque-type TestCase test-case? rackunit)
(provide TestCase test-case?)

(require/typed
 rackunit/private/monad
 [#:opaque monad monad?])
(define-type Seed (U #f monad (Object)))

(define-type test-suite-handler-down
  (rackunit-test-suite (Option String) (Thunk Any) (Thunk Any) Seed -> Seed))
(define-type test-suite-handler-up
  (rackunit-test-suite (Option String) (Thunk Any) (Thunk Any) Seed Seed -> Seed))
(define-type test-suite-handler-here
  (rackunit-test-case (Option String) (Thunk Any) Seed -> Seed))

(require/typed
 rackunit
 [#:struct rackunit-test-case ([name : (Option String)] [action : (Thunk Any)])
           #:constructor-name make-rackunit-test-case]
 [#:struct rackunit-test-suite
           ([name : String]
            [tests : (test-suite-handler-down
                      test-suite-handler-up
                      test-suite-handler-here
                      Seed -> Seed)]
            [before : (Thunk Any)]
            [after : (Thunk Any)])
           #:constructor-name make-rackunit-test-suite])
(require/typed
 rackunit/private/test-suite
 [apply-test-suite (rackunit-test-suite
                    test-suite-handler-down
                    test-suite-handler-up
                    test-suite-handler-here
                    Seed -> Seed)])

(define current-seed : (Parameter Seed)
  (make-parameter #f))

; taken directly from rackunit/private/test-suite
(: test-suite-test-case-around (test-suite-handler-here -> ((Thunk Any) -> Void)))
(define (test-suite-test-case-around fhere)
  (lambda (thunk)
    (let* ([name (current-test-name)]
           [test (make-rackunit-test-case name thunk)]
           [seed (current-seed)])
      (current-seed (fhere test name thunk seed)))))

; taken directly from rackunit/private/test-suite
(: test-suite-check-around (test-suite-handler-here -> ((Thunk Any) -> Void)))
(define (test-suite-check-around fhere)
  (lambda (thunk)
    (let* ([name #f]
           [test (make-rackunit-test-case name thunk)]
           [seed (current-seed)])
      (current-seed (fhere test name thunk seed)))))

; adapted from rackunit/private/test-suite
(define-syntax (test-suite stx)
  (syntax-parse stx
    [(_ name:expr
        (~or (~seq #:before before:expr) (~seq))
        (~or (~seq #:after after:expr) (~seq))
        test:expr ...)
     (with-syntax ([before (if (attribute before) #'before #'void)]
                   [after (if (attribute after) #'after #'void)])
       #'(let ([tests
                : (test-suite-handler-down
                   test-suite-handler-up
                   test-suite-handler-here
                   Seed -> Seed)
                (lambda (fdown fup fhere seed)
                  (define (run/inner [x : Any]) : Any
                    (cond [(rackunit-test-suite? x)
                           (current-seed
                            (apply-test-suite x fdown fup fhere (current-seed)))]
                          [(list? x)
                           (for-each run/inner x)]
                          [else
                           (void)]))
                  (parameterize
                      ([current-seed seed]
                       [current-test-case-around (test-suite-test-case-around fhere)]
                       [current-check-around (test-suite-check-around fhere)])
                    (let ([t : Any test])
                      (run/inner t))
                    ...
                    (current-seed)))])
           (make-rackunit-test-suite
            (ann name : String)
            tests
            (ann before : (Thunk Any))
            (ann after : (Thunk Any)))))]))
(provide test-suite)

(define-type TestSuite rackunit-test-suite)
(provide TestSuite (rename-out [rackunit-test-suite? test-suite?]))

(define-type Test (U TestCase TestSuite))
(provide Test)

(require/typed/provide
 rackunit
 [make-test-suite
  (String (Listof (U TestCase TestSuite)) [#:before (Thunk Any)] [#:after (Thunk Any)] -> TestSuite)])

(require (only-in rackunit define-test-suite define/provide-test-suite))
(provide define-test-suite define/provide-test-suite)

(require/typed/provide
 rackunit
 [current-test-name (Parameter (Option String))]
 [current-test-case-around (Parameter ((Thunk Any) -> Any))])

; 3.3.1.1
(provide test-check test-pred test-equal? test-eq? test-eqv? test-=
         test-true test-false test-not-false test-exn test-not-exn)

(define-syntax-rule (test-check name operator v1 v2)
  (test-case name (check operator v1 v2)))
(define-syntax-rule (test-pred name pred v)
  (test-case name (check-pred pred v)))
(define-syntax-rule (test-equal? name v1 v2)
  (test-case name (check-equal? v1 v2)))
(define-syntax-rule (test-eq? name v1 v2)
  (test-case name (check-eq? v1 v2)))
(define-syntax-rule (test-eqv? name v1 v2)
  (test-case name (check-eqv? v1 v2)))
(define-syntax-rule (test-= name v1 v2 epsilon)
  (test-case name (check-= v1 v2 epsilon)))
(define-syntax-rule (test-true name v)
  (test-case name (check-true v)))
(define-syntax-rule (test-false name v)
  (test-case name (check-false v)))
(define-syntax-rule (test-not-false name v)
  (test-case name (check-not-false v)))
(define-syntax-rule (test-exn name pred thunk)
  (test-case name (check-exn pred thunk)))
(define-syntax-rule (test-not-exn name thunk)
  (test-case name (check-not-exn thunk)))

; 3.4
(require (only-in rackunit before after around delay-test))
(provide before after around delay-test)

; 3.5
; XXX require/expose seems WRONG for typed/scheme

; 3.7
(require-typed-struct (exn:test exn) () rackunit)
(require-typed-struct (exn:test:check exn:test) ([stack : (Listof CheckInfo)]) rackunit)
(require-typed-struct test-result ([test-case-name : (Option String)]) rackunit)
(require-typed-struct (test-failure test-result) ([result : Any]) rackunit)
(require-typed-struct (test-error test-result) ([result : Any]) rackunit)
(require-typed-struct (test-success test-result) ([result : Any]) rackunit)
(provide (struct-out exn:test) (struct-out exn:test:check)
         (struct-out test-result)
         (struct-out test-failure) (struct-out test-error) (struct-out test-success))

(define-type (Tree A)
  (Rec The-Tree
       (Listof (U A The-Tree))))

(require/typed/provide
 rackunit
 [run-test-case
  ((Option String) (Thunk Any) -> test-result)]
 [run-test
  (Test -> (Tree test-result))]
 ; XXX Requires keywords and weird stuff
 #;[fold-test-results
    XXX]
 ; XXX Requires knowing more about test cases and structs
 #;[foldts-test-suite
    XXX])


; 5.1
(require/typed/provide
 rackunit
 [current-check-handler
  (Parameter (-> (U (Rec flat
                      (U Boolean Complex Char
                         Null Symbol String
                         Keyword (Pairof flat flat)))
                 exn)
                 Any))]
 [current-check-around
  (Parameter ((Thunk Any) -> Any))])


