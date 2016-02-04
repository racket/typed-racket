#lang racket/base

;; Tests for type-out
;; - submodules test different type-out forms, these should all compile

;; -----------------------------------------------------------------------------
;; basics / rename

;; type-out a single definition
(module single typed/racket/base
  (provide
    (type-out [f (-> Natural Natural)]))

  (define (f n)
    (define n-1 (- n 1))
    (if (positive? n-1) (* n (f n-1)) 1)))
(require 'single)

;; type-out multiple definitions, along with ordinary provides
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
(require 'multi)

;; use rename form
(module rename typed/racket/base
  (provide
    (type-out
      [rename fact g (-> Natural Natural)]))

  (define (fact n)
    (define n-1 (- n 1))
    (if (positive? n-1) (* n (fact n-1)) 1)))
(require 'rename)

;; -----------------------------------------------------------------------------
;; begin-for-syntax
;; Can 'type-out' at a different phase level

(module bfs typed/racket/base
  (require (for-syntax typed/racket/base))

  (begin-for-syntax
    (provide
      (type-out (f (-> String Boolean))))
    (define (f s)
      (= 8 (string-length s)))))
(require 'bfs)

;; -----------------------------------------------------------------------------
;; struct

;; basic struct definitions, order of declarations does not matter
(module defstruct-1 typed/racket/base
  (struct foo-1 ([a : Natural] [b : (-> Boolean String)]))
  (provide
    (type-out [struct foo-1 ([a : Natural] [b : (-> Boolean String)])]))
  (define f foo-1))
(require 'defstruct-1)

(module defstruct-2 typed/racket/base
  (provide
    (type-out [struct foo-2 ([a : Natural] [b : (-> Boolean String)])]))
  (struct foo-2 ([a : Natural] [b : (-> Boolean String)]))
  (define f foo-2))
(require 'defstruct-2)

;; struct with parent
(module struct/parent typed/racket/base
  (provide (type-out
    (struct bar ([x : Natural] [y : Boolean]))
    (struct (baz bar) ([x : Natural] [y : Boolean] [z : MyType]))))
  (define-type MyType (-> Natural Boolean String))
  (struct bar ([x : Natural] [y : Boolean]))
  (struct baz bar ([z : MyType]) #:property prop:procedure (struct-field-index z)))
(require 'struct/parent)

;; struct, #:omit-constructor
(module omit-constructor-1 typed/racket/base
  (struct qux ([x : Natural] [y : Boolean]))
  (provide (type-out
    (struct qux ([x : Natural] [y : Boolean]) #:omit-constructor))))
(require 'omit-constructor-1)

;; compatible with type-name
(module type-name typed/racket/base
  (provide
   (type-out (struct secret ([x : Key]))))
  (define-type Key String)
  (struct secret ([x : Key]) #:type-name SecretKey))
(require 'type-name)

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

;; -----------------------------------------------------------------------------
;; Example class

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
(require 'class)

;; -----------------------------------------------------------------------------
;; test compatibility with #:opaque types
;; (to check if reordering type declarations breaks things)

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
(require 'opaque-1)

(module opaque-2 typed/racket/base
  (define-type MyTuple (Pairof Str Boolean))

  (require/typed racket/base
   [#:opaque Str string?]
   [string-length (-> Str Natural)])

  (define (b x)
    #t))
(require 'opaque-2)

(module opaque-3 typed/racket/base
  (define-type Foobar (-> Pict))

  (require/typed pict
   [#:opaque Pict pict?]
   [blank (-> Real Real Pict)])

  (provide (type-out
    (c (-> Pict Boolean))))

  (define (c x)
    #t))
(require 'opaque-3)

;;; -----------------------------------------------------------------------------
;; compatible with #:constructor-name

(module constr-name-1 typed/racket/base
  (provide (type-out
    (struct s ())))
  (struct s () #:constructor-name makes)
  (define f makes))
(require 'constr-name-1)

(module constr-name-2 typed/racket/base
  (provide (type-out
    (struct r () #:omit-constructor)))
  (struct r () #:constructor-name maker)
  (define f maker))
(require 'constr-name-2)
