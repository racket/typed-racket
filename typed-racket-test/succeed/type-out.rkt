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
;; struct

;; basic struct definition
(module defstruct typed/racket/base
  (provide
    (type-out
      [struct foo ([a : Natural] [b : (-> Boolean String)])]))
  (define f foo))
(require 'defstruct)

;; compatible with struct #:type-name
;; (but not cooperative -- need to provide new name explicitly)
(module defstruct/type-name typed/racket/base
  (provide
    Bar
    (type-out
      [struct bar () #:type-name Bar])))
(module defstruct/type-name-user typed/racket/base
  (require (submod ".." defstruct/type-name))
  (: barry Bar)
  (define barry (bar)))
(require 'defstruct/type-name-user)

;; struct with parent
(module struct/parent typed/racket/base
  (provide (type-out
    (struct bar ([x : Natural] [y : Boolean]))
    (struct baz bar ([z : MyType]))
  ))
  (define-type MyType (-> Natural Boolean String)))
(require 'struct/parent)

;; struct, #:omit-constructor
(module omit-constructor-1 typed/racket/base
  (provide (type-out
    (struct qux ([x : Natural] [y : Boolean]) #:omit-constructor))))
(require 'omit-constructor-1)

;; can re-order #:omit-constructor relative to other options
(module omit-constructor-2 typed/racket/base
  (provide (type-out
    (struct quux () #:type-name Quux #:omit-constructor)
    (struct quuux () #:omit-constructor #:type-name Quuux))))
(require 'omit-constructor-2)

;; -----------------------------------------------------------------------------
;; type

(module deftype typed/racket/base
  (provide (type-out
    (type Person (Pairof String Natural))
    (person<? (-> Person Person Boolean))))
  (define (person<? p1 p2)
    (string<? (car p1) (car p2))))
(require 'deftype)

(module deftype-user typed/racket/base
  (require (submod ".." deftype))
  (provide person<?*)

  (define-type People (Listof Person))

  (: person<?* (-> People People Boolean))
  (define (person<?* p1* p2*)
    (for/fold : Boolean
              ([acc #t])
              ([p1 (in-list p1*)]
               [p2 (in-list p2*)])
      (and acc (person<? p1 p2)))))
(require 'deftype-user)

;; -----------------------------------------------------------------------------
;; test compatibility with #:opaque types
;; (to check if reordering type declarations breaks things)

(module opaque-1 typed/racket/base
  (require/typed racket/base
   [#:opaque Str string?]
   [string-length (-> Str Natural)])

  (provide (type-out
    (type MyTuple (Pairof String Natural))
    (a (-> MyTuple Boolean))))

  (define (a x)
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

;; -----------------------------------------------------------------------------
;; compatible with #:constructor-name

(module constr-name-1 typed/racket/base
  (provide (type-out
    (struct s () #:constructor-name makes)))
  makes)
(require 'constr-name-1)

(module constr-name-2 typed/racket/base
  (provide (type-out
    (struct r () #:omit-constructor #:constructor-name maker)))
  maker)
(require 'constr-name-2)
