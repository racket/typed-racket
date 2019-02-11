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
         racket/lazy-require
         (for-syntax racket/base racket/syntax
                     syntax/parse))

(provide Type Type?
         Prop Prop?
         Object Object? OptObject?
         PathElem PathElem?
         SomeValues SomeValues?
         def-type
         def-values
         def-prop
         def-object
         def-path-elem)


;;************************************************************
;; Custom Printing Tools
;;************************************************************

(lazy-require
 ["../types/printer.rkt" (print-type
                          print-prop print-object print-pathelem
                          print-values print-propset print-result)])

;; comment out the above lazy-require and uncomment the following
;; s-exp for simple debug printing of Rep structs
#;(begin
    (define (debug-printer rep port write?)
      (display (cons (Rep-name rep) (Rep-values rep)) port))

    (define print-type debug-printer)
    (define print-prop debug-printer)
    (define print-object debug-printer)
    (define print-pathelem debug-printer)
    (define print-values debug-printer)
    (define print-propset debug-printer)
    (define print-result debug-printer))

(define-syntax (def-rep-class stx)
  (syntax-parse stx
    [(_ name:id
        #:printer printer:id
        #:define-form def:id)
     (with-syntax ([mk (generate-temporary 'dont-use-me)])
       (quasisyntax/loc
           stx
         (begin (struct name ()
                  #:constructor-name mk
                  #:transparent
                  #:property prop:custom-print-quotable 'never
                  #:methods gen:custom-write
                  ;; Note: We eta expand the printer so it is not evaluated until needed.
                  [(define (write-proc v port write?) (printer v port write?))])
                (define-syntax (def stx)
                  (syntax-parse stx
                    [(_ variant:id flds:expr . rst)
                     (syntax/loc stx
                       (def-rep variant flds [#:parent name] . rst))])))))]))

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


(def-rep-class Type #:printer print-type #:define-form def-type)

;;-----------------
;; Universal Type
;;-----------------

;; the type of all well-typed terms
;; (called Any in user programs)
(def-type Univ ()
  [#:mask mask:unknown]
  [#:singleton Univ])

;;-----------------
;; Bottom Type
;;-----------------

(def-type Bottom ()
  [#:mask mask:bottom]
  [#:singleton -Bottom])


;;************************************************************
;; Prop
;;************************************************************
;;
;; These convey learned information about program terms while
;; typechecking.
(def-rep-class Prop #:printer print-prop #:define-form def-prop)

(def-prop TrueProp () [#:singleton -tt])
(def-prop FalseProp () [#:singleton -ff])

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
(def-rep-class PathElem #:printer print-pathelem #:define-form def-path-elem)


;;----------
;; Objects
;;----------
(def-rep-class Object #:printer print-object #:define-form def-object)

;; empty object
(def-rep Empty ()
  [#:singleton -empty-obj]
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
(def-rep-class SomeValues #:printer print-values #:define-form def-values)


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
  [#:frees (f) (combine-frees (list (f thn) (f els)))]
  [#:fmap (f) (make-PropSet (f thn) (f els))]
  [#:for-each (f) (begin (f thn) (f els))]
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
  [#:frees (f) (combine-frees (list (f t) (f ps) (f o)))]
  [#:fmap (f) (make-Result (f t) (f ps) (f o))]
  [#:for-each (f) (begin (f t) (f ps) (f o))]
  [#:extras
   #:property prop:custom-print-quotable 'never
   #:methods gen:custom-write
   [(define (write-proc v port write?) (print-result v port write?))]])
