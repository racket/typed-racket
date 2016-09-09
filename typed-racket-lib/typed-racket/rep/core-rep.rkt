#lang racket/base

;;************************************************************
;; core-rep.rkt
;;
;; In this file we define the parent structs that describe most of
;; Typed Racket's internal forms and define a few variants which are
;; referenced in many definitions.
;; ************************************************************


(require "../utils/utils.rkt"
         "rep-utils.rkt"
         "free-variance.rkt"
         "type-mask.rkt"
         (contract-req)
         racket/match
         racket/list
         racket/lazy-require
         (for-syntax racket/base racket/syntax
                     syntax/parse))

(provide Type Type-mask Type-subtype-cache Type?
         Prop Prop?
         Object Object? OptObject?
         PathElem PathElem?
         SomeValues SomeValues?
         def-type
         def-values
         def-prop
         def-object
         def-pathelem
         type-equal?
         prop-equal?
         object-equal?)

(define-syntax type-equal? (make-rename-transformer #'eq?))
(define-syntax prop-equal? (make-rename-transformer #'eq?))
(define-syntax object-equal? (make-rename-transformer #'eq?))

(provide-for-cond-contract name-ref/c)

;; A Name-Ref is any value that represents an object.
;; As an identifier, it represents a free variable in the environment
;; As a pair, it represents a De Bruijn indexed bound variable (cons lvl arg-num)
(define-for-cond-contract name-ref/c
  (or/c identifier? (cons/c natural-number/c natural-number/c)))

;;************************************************************
;; Custom Printing Tools
;;************************************************************

(lazy-require
 ["../types/printer.rkt" (print-type
                          print-prop print-object print-pathelem
                          print-values print-propset print-result)])

;; Note: We eta expand the printer so it is not evaluated until needed.
(define-syntax (struct/printer stx)
  (syntax-parse stx
    [(_ name:id
        (flds:id ...)
        printer:id)
     (with-syntax ([mk (generate-temporary 'dont-use-me)])
       (syntax/loc
           stx
         (struct name Rep (flds ...)
           #:constructor-name mk
           #:transparent
           #:property prop:custom-print-quotable 'never
           #:methods gen:custom-write
           [(define (write-proc v port write?) (printer v port write?))])))]))


;;
;; These structs are the 'meta-variables' of TR's internal grammar,
;; if you will. For reference, see the following two papers which
;; discuss Typed Racket's metatheory:
;;
;; 1. Logical Types for Untyped Languages, Tobin-Hochstadt &
;; Felleisen, ICFP 2010
;;
;; 2. Occurrence Typing Modulo Theories, Kent et al., PLDI 2016

;;************************************************************
;; Types
;;************************************************************
;;
;;
;; The 'mask' field that is used for quick-checking of certain
;; properties. See type-mask.rkt for details.

(struct/printer Type (subtype-cache mask) print-type)

(define-syntax (def-type stx)
  (syntax-parse stx
    [(_ variant:id flds:expr . rst)
     (syntax/loc stx
       (def-rep variant flds [#:parent Type] . rst))]))

;;-----------------
;; Universal Type
;;-----------------

;; the type of all well-typed terms
;; (called Any in user programs)
(def-type Univ () #:base
  [#:type-mask mask:unknown])

;;-----------------
;; Bottom Type
;;-----------------

(def-type Bottom () #:base
  [#:type-mask mask:bottom])

;;************************************************************
;; Prop
;;************************************************************
;;
;; These convey learned information about program terms while
;; typechecking.

(struct/printer Prop () print-prop)

(define-syntax (def-prop stx)
  (syntax-parse stx
    [(_ variant:id flds:expr . rst)
     (syntax/loc stx
       (def-rep variant flds [#:parent Prop] . rst))]))

(def-prop TrueProp () #:base)

(def-prop FalseProp () #:base)

;;************************************************************
;; Fields and Symbolic Objects
;;************************************************************
;;
;; These are used to represent the class of canonical program terms
;; that can be lifted to the type level while typechecking.


;;--------------
;; PathElements
;;--------------

;; e.g. car, cdr, etc
(struct/printer PathElem () print-pathelem)

(define-syntax (def-pathelem stx)
  (syntax-parse stx
    [(_ variant:id flds:expr . rst)
     (syntax/loc stx
       (def-rep variant flds [#:parent PathElem] . rst))]))


;;----------
;; Objects
;;----------

(struct/printer Object () print-object)

(define-syntax (def-object stx)
  (syntax-parse stx
    [(_ variant:id flds:expr . rst)
     (syntax/loc stx
       (def-rep variant flds [#:parent Object]  . rst))]))

;; empty object
(def-rep Empty () #:base
  [#:extras
   #:property prop:custom-print-quotable 'never
   #:methods gen:custom-write
   [(define (write-proc v port write?)
      (when write?
        (write-string "-" port)))]])

(define/provide (OptObject? x)
  (or (Object? x) (Empty? x)))


;;************************************************************
;; SomeValues
;;************************************************************
;;
;; Racket expressions can produce 0 or more values, 'SomeValues'
;; represents the general class of all these possibilities

(struct/printer SomeValues () print-values)

(define-syntax (def-values stx)
  (syntax-parse stx
    [(_ variant:id flds:expr . rst)
     (syntax/loc stx
       (def-rep variant flds [#:parent SomeValues] . rst))]))



;;************************************************************
;; PropSets
;;************************************************************
;; These are a convenient way to pair 'then' and 'else' propositions
;; together, which appear in typechecking results and in function
;; types.
;;
;; Since there is only one form, we do not define an empty parent
;; struct that other structs inherit from.


(def-rep PropSet ([thn Prop?] [els Prop?])
  [#:intern-key (cons (Rep-seq thn) (Rep-seq els))]
  [#:frees (f) (combine-frees (list (f thn) (f els)))]
  [#:fold (f) (make-PropSet (f thn) (f els))]
  [#:walk (f) (begin (f thn) (f els))]
  [#:extras
   #:property prop:custom-print-quotable 'never
   #:methods gen:custom-write
   [(define (write-proc v port write?) (print-propset v port write?))]])



;;************************************************************
;; Results
;;************************************************************
;;
;; These represent all the relevant info derived from typechecking a
;; term which produces one value, namely it's type (t), what is learned
;; if it is used as an 'if' test expression (ps), and what, if any, symbolic
;; object the value would correspond to (o).
;;
;; Since there is only one form, we do not define an empty parent
;; struct that other structs inherit from.


(def-rep Result ([t Type?] [ps PropSet?] [o OptObject?])
  [#:intern-key (list* (Rep-seq t) (Rep-seq ps) (Rep-seq o))]
  [#:frees (f) (combine-frees (list (f t) (f ps) (f o)))]
  [#:fold (f) (make-Result (f t) (f ps) (f o))]
  [#:walk (f) (begin (f t) (f ps) (f o))]
  [#:extras
   #:property prop:custom-print-quotable 'never
   #:methods gen:custom-write
   [(define (write-proc v port write?) (print-result v port write?))]])

