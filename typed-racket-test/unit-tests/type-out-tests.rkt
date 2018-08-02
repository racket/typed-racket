#lang racket/base

;; Tests for type-out.
;; Each test is written as some Racket symbols to be eval'd in a fresh namespace.
;; Use `eval/pass DATA` and `eval/fail rx DATA` to test

(provide tests)
(require rackunit)

;; ------------------------------------------------------------------------------------------------------
;; utilities

(define (eval/pass e)
  (check-not-exn
    (lambda () (eval e (make-base-namespace)) (void))))

(define (eval/fail exn-regexp e)
  (check-exn exn-regexp
    (lambda () (eval e (make-base-namespace)))))

(define tests (make-test-suite "type-out tests" (list
  (test-suite "basics / (type-out ... rename ...)" ;; -------------------------
    ;; type-out a single definition
    (eval/pass
      '(begin
        (module single typed/racket/base
          (provide
            (type-out [f (-> Natural Natural)]))
          (define (f n)
            (define n-1 (- n 1))
            (if (positive? n-1) (* n (f n-1)) 1)))
        (require 'single)))

    ;; type-out multiple definitions, along with ordinary provides
    (eval/pass
      '(begin
        (module multi typed/racket/base
          (provide
            (type-out [n Natural]
                      [fact (-> Natural Natural)])
            fib)

          (define n 12)

          (define (fact n)
            (define n-1 (- n 1))
            (if (positive? n-1) (* n (fact n-1)) 1))

          (: fib (-> Natural Natural))
          (define (fib n)
            (define n-1 (- n 1))
            (define n-2 (- n 2))
            (if (and (positive? n-1) (positive? n-2))
              (+ (fib n-1) (fib n-2)) 1)))
        (require 'multi)))

    ;; use rename form
    (eval/pass
      '(begin
        (module rename typed/racket/base
          (provide
            (type-out
              [rename fact g (-> Natural Natural)]))

          (define (fact n)
            (define n-1 (- n 1))
            (if (positive? n-1) (* n (fact n-1)) 1)))
        (require 'rename)))

    ;; (type-out (rename ...))
    ;; Does not provide original identifier
    (eval/fail #rx"f: undefined"
      '(begin
        (module rename typed/racket/base
          (provide
            (type-out (rename f g (-> Natural Natural))))
          (define (f n)
            (let ([n-1 (- n 1)])
              (if (positive? n-1) (* n (f n-1)) 1))))
        (require 'rename)
        (f 4)))

    ;; Type mismatch
    (eval/fail #rx"Type Checker"
      '(begin
        (module bad-type-1 typed/racket/base
          (provide
            (type-out (n String)))
          (define n 3))))

    (eval/fail #rx"Type Checker"
      '(begin
        (module bad-type-2 typed/racket/base
          (provide
            (type-out (n Natural)))
          (define n -3))))

    ;; Arity mismatch
    (eval/fail #rx"Type Checker"
      '(begin
        (module bad-arity typed/racket/base
          (provide
            (type-out (f (-> Natural Natural))))
          (define (f x y) y))))
  )
  (test-suite "type-out/phase level change" ;; --------------------------------

    (eval/pass
      '(begin
        (module bfs typed/racket/base
          (require (for-syntax typed/racket/base))

          (begin-for-syntax
            (provide
              (type-out (f (-> String Boolean))))
            (define (f s)
              (= 8 (string-length s)))))
        (require 'bfs)))

    ;; Cannot type-out at a different phase from the provide
    (eval/fail #rx"Type Checker.*`s' has no definition"
      '(begin
        (module for-stx typed/racket/base
          (require (for-syntax typed/racket/base))

          (provide
            (for-syntax (type-out [s (-> String String)])))

          (define-for-syntax (s str) ""))
        (require 'for-stx)))
  )
  (test-suite "struct tests" ;; -----------------------------------------------

    ;; basic struct definitions, order of declarations does not matter
    (eval/pass
      '(begin
        (module defstruct-1 typed/racket/base
          (struct foo-1 ([a : Natural] [b : (-> Boolean String)]))
          (provide
            (type-out [struct foo-1 ([a : Natural] [b : (-> Boolean String)])]))
          (define f foo-1))
        (require 'defstruct-1)))

    (eval/pass
      '(begin
        (module defstruct-2 typed/racket/base
          (struct foo-2 ([a : Natural] [b : (-> Boolean String)]))
          (provide
            (type-out [struct foo-2 ([a : Natural] [b : (-> Boolean String)])]))
          (define f foo-2))
        (require 'defstruct-2)
        foo-2?
        foo-2-a))

    ;; Order of field definitions is enforced
    (eval/fail #rx"type-out: expected annotation"
      '(begin
        (module defstruct-3 typed/racket/base
          (struct foo-3 ([b : (-> Boolean String)] [a : Natural]))
          (provide
            (type-out [struct foo-3 ([a : Natural] [b : (-> Boolean String)])]))
          (define f foo-3))
        (require 'defstruct-3)))

    ;; struct with parent, provide both struct & parent
    (eval/pass
      '(begin
        (module struct/parent typed/racket/base
          (struct bar ([x : Natural] [y : Boolean]))
          (struct baz bar ([z : MyType]) #:property prop:procedure (struct-field-index z))
          (provide (type-out
            (struct bar ([x : Natural] [y : Boolean]))
            (struct (baz bar) ([z : MyType]))))
          (define-type MyType (-> Natural Boolean String)))
        (require 'struct/parent)
        bar? baz?))

    ;; struct with parent, struct only
    (eval/pass
      '(begin
        (module struct/parent typed/racket/base
          (struct bar ([x : Natural] [y : Boolean]))
          (struct baz bar ([z : MyType]) #:property prop:procedure (struct-field-index z))
          (provide (type-out
            (struct (baz bar) ([z : MyType]))))
          (define-type MyType (-> Natural Boolean String)))
        (require 'struct/parent)
        baz?))

    ;; Check provides after an #:omit-constructor
    (eval/pass
      '(begin
        (module omit-constructor-1 typed/racket/base
          (struct qux ([x : Natural] [y : Boolean]))
          (provide (type-out
            (struct qux ([x : Natural] [y : Boolean]) #:omit-constructor))))
        (require 'omit-constructor-1)
        qux?))

    ;; #:omit-constructor
    (eval/fail #rx"qux: undefined"
      '(begin
        (module omit-constructor-1 typed/racket/base
          (struct qux ([x : Natural] [y : Boolean]))
          (provide (type-out
            (struct qux ([x : Natural] [y : Boolean]) #:omit-constructor))))
        (require 'omit-constructor-1)
        (qux 0 #t)))

    ;; #:omit-constructor should hide #:constructor-name constructors as well
    (eval/fail #rx"makes: undefined"
      '(begin
        (module constr-name typed/racket/base
          (struct s () #:constructor-name makes)
          (provide (type-out
            (struct s () #:omit-constructor))))
        (require 'constr-name)
        makes))

    ;; #:omit-constructor should hide #:extra-constructor-name too
    (eval/fail #rx"makes: undefined"
      '(begin
        (module constr-name typed/racket/base
          (struct s () #:extra-constructor-name makes)
          (provide (type-out
            (struct s () #:omit-constructor))))
        (require 'constr-name)
        makes))

    ;; #:extra-constructor-name also hides default constructor
    (eval/fail #rx"s: undefined"
      '(begin
        (module constr-name typed/racket/base
          (struct s () #:extra-constructor-name makes)
          (provide (type-out
            (struct s () #:omit-constructor))))
        (require 'constr-name)
        (s)))

    ;; Must define the struct before providing it
    (eval/fail #rx"type-out: unknown struct type"
      '(begin
        (module defstruct/type-name typed/racket/base
          (provide
            (type-out [struct bar ()])))))

    ;; Must define the struct before providing it
    (eval/fail #rx"type-out: unknown struct type"
      '(begin
        (module defstruct/type-name typed/racket/base
          (define bar 1)
          (provide
            (type-out [struct bar ()])))))

    ;; Test incorrect field type
    (eval/fail #rx"Type Checker"
      '(begin
        (module s typed/racket/base
          (struct s ([f : (-> Boolean String)]))
          (provide
            (type-out (struct s ([f : Boolean])))))
        (require 's)))

    ;; Must declare all struct fields
    (eval/fail #rx"type-out: missing annotation"
      '(begin
        (module s typed/racket/base
          (struct s ([f : (-> Boolean String)]))
          (provide
            (type-out (struct s ()))))
        (require 's)
        (define my-s (s (lambda (x) "hi")))
        ((s-f my-s) #t)))

    ;; Must declare all struct fields, but not parent fields
    (eval/fail #rx"type-out: struct field does not exist"
      '(begin
        (module t typed/racket/base
          (struct bar ([x : Natural]))
          (struct baz bar ([y : Boolean]))
          (provide (type-out
            (struct bar ([x : Natural]))
            (struct (baz bar) ([y : Boolean] [x : Natural])))))))

    ;; Must declare all struct fields, but not parent fields or grandparent fields
    (eval/fail #rx"type-out: struct field does not exist"
      '(begin
        (module t typed/racket/base
          (struct bar ([x : Natural]))
          (struct baz bar ([y : Boolean]))
          (struct qux baz ([z : String] [x : Natural]))
          (provide (type-out
            (struct bar ([x : Natural]))
            (struct (baz bar) ([y : Boolean] [x : Natural]))
            (struct (qux baz) ([z : String] [y : Boolean])))))))

    ;; Must really be a sub-struct of parent
    (eval/fail #rx"type-out: struct type baz is not a subtype"
      '(begin
        (module t typed/racket/base
          (struct bar ([x : Natural]))
          (struct baz ([y : Boolean]))
          (provide (type-out
            (struct bar ([x : Natural]))
            (struct (baz bar) ([y : Boolean])))))))

    ;; type-out should not declare extra fields
    (eval/fail #rx"type-out: struct field does not exist"
      '(begin
        (module s typed/racket/base
          (struct s ([f : (-> Boolean String)]))
          (provide
            (type-out (struct s ([f : (-> Boolean String)]
                                 [g : Natural])))))
        (require 's)))

    ;; Test bad struct option in type-out
    (eval/fail #rx"type-out: expected the literal"
      '(begin
        (module struct-def typed/racket/base
         (struct A ((a : String)) #:type-name Foo)
         (provide (type-out
           (struct A ((a : String)) #:type-name Foo))))))
    ;; Works with hyphenated struct/field names
    (eval/pass
      '(begin
        (module t typed/racket/base
          (struct a-b ([c-d : Natural]))
          (provide (type-out
            (struct a-b ([c-d : Natural])))))
        (require 't)))
  )
  (test-suite "class" ;; ------------------------------------------------------

    (eval/pass
      '(begin
        (module class typed/racket/base
          (provide (type-out
            (automaton% Automaton)
            (a oAutomaton)))

          ;; Example OO code
          (require typed/racket/class)

          (define-type State Natural)
          (define-type Payoff Natural)
          (define-type Transition* (Vectorof (Vectorof Payoff)))
          (define-type oAutomaton (Instance Automaton))
          (define-type Automaton
            (Class
             (init-field [current State]
                         [payoff Payoff]
                         [table Transition*]
                         [original State #:optional])
             [match-pair (-> oAutomaton Natural (values oAutomaton oAutomaton))]
             [jump (-> State Payoff Void)]
             [pay (-> Payoff)]
             [reset (-> oAutomaton)]
             [clone (-> oAutomaton)]
             [equal (-> oAutomaton Boolean)]))

          (define automaton%
            (let ()
              (class object%
                (init-field
                 current
                 payoff
                 table
                 (original current))
                (super-new)

                (define/public (match-pair other r)
                  ;; Implementation omitted
                  (values this other))

                (define/public (jump input delta)
                  (set! current (vector-ref (vector-ref table current) input))
                  (set! payoff (+ payoff delta)))

                (define/public (pay)
                  payoff)

                (define/public (reset)
                  (new automaton% [current original][payoff 0][table table]))

                (define/public (clone)
                  (new automaton% [current original][payoff 0][table table]))

                (: compute-payoffs (-> State [cons Payoff Payoff]))
                (define/private (compute-payoffs other-current)
                  (vector-ref (vector-ref #(#()) current) other-current))

                (define/public (equal other)
                  (and (= current (get-field current other))
                       (= original (get-field original other))
                       (= payoff (get-field payoff other))
                       (equal? table (get-field table other)))))))

          (define a (new automaton% (current 0) (payoff 999) (table '#(#(0 0) #(1 1))))))
        (require 'class)))
  )
  (test-suite "#:opaque" ;; ---------------------------------------------------

    (eval/pass
      '(begin
        (module opaque-1 typed/racket/base
          (require/typed racket/base
           [#:opaque Str string?]
           [string-length (-> Str Natural)])

          (provide
            MyTuple
            (type-out (and-cdr (-> MyTuple Boolean))))
          (define-type MyTuple (Pairof String Natural))

          (define (and-cdr x)
            (and (cdr x) #t)))
        (require 'opaque-1)))

    (eval/pass
      '(begin
        (module opaque-2 typed/racket/base
          (define-type MyTuple (Pairof Str Boolean))

          (require/typed racket/base
           [#:opaque Str string?]
           [string-length (-> Str Natural)])

          (define (b x)
            #t))
        (require 'opaque-2)))

    (eval/pass
      '(begin
        (module opaque-3 typed/racket/base
          (define-type Foobar (-> Pict))

          (require/typed pict
           [#:opaque Pict pict?]
           [blank (-> Real Real Pict)])

          (provide (type-out
            (c (-> Pict Boolean))))

          (define (c x)
            #t))
        (require 'opaque-3)))
  )
  (test-suite "#:constructor-name" ;; -----------------------------------------

    (eval/pass
      '(begin
        (module constr-name-1 typed/racket/base
          (struct s () #:constructor-name makes)
          (provide (type-out
            (struct s ())))
          (define f makes))
        (require 'constr-name-1)
        (s? (makes))))

    (eval/pass
      '(begin
        (module constr-name-2 typed/racket/base
          (struct r () #:constructor-name maker)
          (provide (type-out
            (struct r () #:omit-constructor)))
          (define f maker))
        (require 'constr-name-2)
        r?))

    (eval/fail #rx"maker: undefined"
      '(begin
        (module constr-name-2 typed/racket/base
          (struct r () #:constructor-name maker)
          (provide (type-out
            (struct r () #:omit-constructor)))
          (define f maker))
        (require 'constr-name-2)
        (maker)))
  )
#;(test-suite "TODO : KNOWN FAILURES" ;; --------------------------------------

    ;; compatible with type-name
    (eval/pass
      '(begin
        (module type-name typed/racket/base
          (struct secret ([x : Key]) #:type-name SecretKey)
          (define-type Key String)
          (provide
           (type-out (struct secret ([x : Key])))))
        (require 'type-name)))

    ;; blocked on https://github.com/racket/typed-racket/issues/312
    (eval/fail #rx"identifier for static struct type information cannot be used as an expression"
      '(begin
        (module constr-name-1 typed/racket/base
          (struct s () #:constructor-name makes)
          (provide (type-out
            (struct s ())))
          (define f (makes)))
        (require 'constr-name-1)
        (s)))

    ;; blocked on https://github.com/racket/typed-racket/issues/304
    ;; compatible with polymorphic type-name?
    (module type-name-poly typed/racket/base
      (struct (A) secret-poly ([x : A]) #:type-name SecretPoly)
      (provide
       (type-out (struct (A) secret-poly ([x : A])))))
    (require 'type-name-poly)
    ; more intense use of type variables
    (module type-var typed/racket/base
      (provide
       (type-out
        (struct (A B C) ski ([S : (-> (-> A B C) (-> A B) A C)]
                             [K : (-> A B A)]
                             [I : (-> A A)]))))
      (struct (A B C) ski ([S : (-> (-> A B C) (-> A B) A C)]
                           [K : (-> A B A)]
                           [I : (-> A A)])
        #:type-name SKI
        #:extra-constructor-name make-SKI
        #:property prop:procedure
          (struct-field-index S)))
    (require 'type-var)
  )
)))
