#lang racket/base

;; Tests for type-out
;; - submodules test different type-out forms, these should all compile

;; ------------------------------------------------------------------------------------------------------
;; utilities

(define (eval/pass e)
  (eval e (make-base-namespace)))

;; TODO not catching syntax errors
(define (eval/fail exn-regexp e)
  (define (check-exn e)
    (if (regexp-match? exn-regexp (exn-message e)) #t (raise e)))
  (unless (with-handlers ([exn? check-exn])
            (begin (eval e (make-base-namespace)) #f))
    (error 'type-out-tests "expression did not raise an exception")))

;; ------------------------------------------------------------------------------------------------------
;; basics / rename

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
(eval/fail "f: unbound identifier"
  '(begin
    (module rename typed/racket/base
      (provide
        (type-out (rename f g (-> Natural Natural))))
      (define (f n)
        (let ([n-1 (- n 1)])
          (if (positive? n-1) (* n (f n-1)) 1))))
    (require 'rename)
    (f 4)))

;; ------------------------------------------------------------------------------------------------------
;; begin-for-syntax
;; Can 'type-out' at a different phase level, as long as it's the same phase as the provide

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
(eval/fail "foobear"
  '(begin
    (module for-stx typed/racket/base
      (require (for-syntax typed/racket/base))

      (provide
        (for-syntax (type-out [s (-> String String)])))

      (define-for-syntax (s str) ""))
    (require 'for-stx)))

;; ------------------------------------------------------------------------------------------------------
;; struct

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
      (provide
        (type-out [struct foo-2 ([a : Natural] [b : (-> Boolean String)])]))
      (struct foo-2 ([a : Natural] [b : (-> Boolean String)]))
      (define f foo-2))
    (require 'defstruct-2)))

;; struct with parent
(eval/pass
  '(begin
    (module struct/parent typed/racket/base
      (provide (type-out
        (struct bar ([x : Natural] [y : Boolean]))
        (struct (baz bar) ([x : Natural] [y : Boolean] [z : MyType]))))
      (define-type MyType (-> Natural Boolean String))
      (struct bar ([x : Natural] [y : Boolean]))
      (struct baz bar ([z : MyType]) #:property prop:procedure (struct-field-index z)))
    (require 'struct/parent)))

;; struct, #:omit-constructor
(eval/pass
  '(begin
    (module omit-constructor-1 typed/racket/base
      (struct qux ([x : Natural] [y : Boolean]))
      (provide (type-out
        (struct qux ([x : Natural] [y : Boolean]) #:omit-constructor))))
    (require 'omit-constructor-1)))

;; #:omit-constructor should hide #:constructor-name constructors
(eval/fail "TODO"
  '(begin
    (module constr-name typed/racket/base
      (struct s () #:constructor-name makes)
      (provide (type-out
        (struct s () #:omit-constructor))))
    (require 'constr-name)
    makes))

;; compatible with type-name
(eval/pass
  '(begin
    (module type-name typed/racket/base
      (provide
       (type-out (struct secret ([x : Key]))))
      (define-type Key String)
      (struct secret ([x : Key]) #:type-name SecretKey))
    (require 'type-name)))

;; Must define the struct before providing it
(eval/fail "TODO"
  '(begin
    (module defstruct/type-name typed/racket/base
      (provide
        (type-out [struct bar ()])))))

;; Test incorrect field type
(eval/fail "TODO"
  '(begin
    (module s typed/racket/base
      (provide
        (type-out (struct s ([f : Boolean]))))
      (struct s ([f : (-> Boolean String)])))
    (require 's)))

;; Must declare all struct fields
(eval/fail "TODO"
  '(begin
    (module s typed/racket/base
      (provide
        (type-out (struct s ())))
      (struct s ([f : (-> Boolean String)])))
    (require 's)
    (define my-s (s (lambda (x) "hi")))
    ((s-f my-s) #t)))

;; Type-out structs need types for each field, including parent fields
(eval/fail "TODO"
  '(begin
    (module t typed/racket/base
      (provide (type-out
        (struct bar ([x : Natural]))
        (struct (baz bar) ([y : Boolean]))))
      (struct bar ([x : Natural]))
      (struct baz bar ([y : Boolean])))))

;; type-out should not declare extra fields
(eval/fail "TODO"
  '(begin
    (module s typed/racket/base
    (provide
      (type-out (struct s ([f : (-> Boolean String)]
                           [g : Natural]))))
    (struct s ([f : (-> Boolean String)])))
    (require 's)))

;; Can only use '#:omit-constructor' in `type-out`
(eval/fail "TODO"
  '(begin
    (module struct-def typed/racket/base
     (provide (type-out
       (struct A ((a : String)) #:type-name Foo)))
     (struct A ((a : String)) #:type-name Foo))))

;; fail: can't use omitted name outside module
(eval/fail "foo: unbound identifier"
  '(begin
    (module omit-constructor typed/racket/base
      (struct foo ())
      (provide
        (type-out (struct foo () #:omit-constructor))))

    (require 'omit-constructor)
    foo))

;; Type name should not be a constructor if #:constructor-name is used
(eval/fail "s: unbound identifier"
  '(begin
    (module constr-name typed/racket/base
      (struct s () #:constructor-name makes)
      (provide (type-out
        (struct s () #:omit-constructor))))
    (require 'constr-name)
    (define my-s (s))))

;; compatible with polymorphic type-name? wait for issue #304
;(module type-name-poly typed/racket/base
;  (struct (A) secret-poly ([x : A]) #:type-name SecretPoly)
;  (provide
;   (type-out (struct (A) secret-poly ([x : A])))))
;(require 'type-name-poly)

;; more intense use of type variables
;(module type-var typed/racket/base
;  (provide
;   (type-out
;    (struct (A B C) ski ([S : (-> (-> A B C) (-> A B) A C)]
;                         [K : (-> A B A)]
;                         [I : (-> A A)]))))
;  (struct (A B C) ski ([S : (-> (-> A B C) (-> A B) A C)]
;                       [K : (-> A B A)]
;                       [I : (-> A A)])
;    #:type-name SKI
;    #:extra-constructor-name make-SKI
;    #:property prop:procedure
;      (struct-field-index S)))
;(require 'type-var)

;; ------------------------------------------------------------------------------------------------------
;; Example class

(eval/pass
  '(begin
    (module class typed/racket/base
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

       (define a (new automaton% (current 0) (payoff 999) (table '#(#(0 0) #(1 1)))))

       (provide (type-out
         (automaton% Automaton)
         (a oAutomaton))))
    (require 'class)))

;; ------------------------------------------------------------------------------------------------------
;; test compatibility with #:opaque types
;; (to check if reordering type declarations breaks things)

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

;; ------------------------------------------------------------------------------------------------------
;; compatible with #:constructor-name

(eval/pass
  '(begin
    (module constr-name-1 typed/racket/base
      (provide (type-out
        (struct s ())))
      (struct s () #:constructor-name makes)
      (define f makes))
    (require 'constr-name-1)))

(eval/pass
  '(begin
    (module constr-name-2 typed/racket/base
      (provide (type-out
        (struct r () #:omit-constructor)))
      (struct r () #:constructor-name maker)
      (define f maker))
    (require 'constr-name-2)))
