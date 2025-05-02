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
         "type-constr.rkt"
         "free-variance.rkt"
         "type-mask.rkt"
         (contract-req)
         racket/match
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
         def-path-elem
         make-ExitentialResult
         Result?
         instantiate-result
         (rename-out [make-Result* make-Result]
                     [ExistentialResult:* ExistentialResult:]
                     [Result:* Result:]))


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
        #:define-form def:id
        (~optional (#:extra extra ...) #:defaults ([(extra 1) null])))
     (with-syntax ([mk (generate-temporary 'dont-use-me)])
       (quasisyntax/loc
           stx
         (begin (struct name ()
                  #:constructor-name mk
                  #:transparent
                  #:property prop:custom-print-quotable 'never
                  extra ...
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


(def-rep-class Type #:printer print-type #:define-form def-type
  (#:extra
   #:property prop:kind #t))

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


(def-rep Result ([t Type?]
                 [ps PropSet?]
                 [o OptObject?]
                 ;; the number of the existential quantifiers
                 [n-existentials number?])
  #:no-provide (make-Result Result:)
  [#:frees (f) (combine-frees (list (f t) (f ps) (f o)))]
  [#:fmap (f) (make-Result (f t) (f ps) (f o) n-existentials)]
  [#:for-each (f) (begin (f t) (f ps) (f o))]
  [#:extras
   #:property prop:custom-print-quotable 'never
   #:methods gen:custom-write
   [(define (write-proc v port write?) (print-result v port write?))]])


(define (make-Result* t ps o [n-existentials 0])
  (make-Result t ps o n-existentials))

(lazy-require ["type-rep.rkt" (abstract-type abstract-propset instantiate-propset instantiate-type make-F)])

(define type-var-name-table (make-hash))

(define/cond-contract (make-ExitentialResult exi-syms t ps o)
  (-> (listof symbol?) Type? PropSet? OptObject? Result?)
  (define v (make-Result (abstract-type t exi-syms)
                         (abstract-propset ps exi-syms)
                         o
                         (length exi-syms)))
  (hash-set! type-var-name-table v exi-syms)
  v)

(define/cond-contract (instantiate-result result)
  (-> Result? Result?)
  (match-define (Result: type propset optobject n-existentials) result)
  (cond
    [(positive? n-existentials)
     (define syms (hash-ref type-var-name-table result (build-list n-existentials (lambda _ (gensym)))))
     (define vars (map make-F syms))
     (make-Result (instantiate-type type vars) (instantiate-propset propset vars) optobject n-existentials)]
    [else result]))

(define-match-expander Result:*
  (lambda (stx)
    (syntax-case stx ()
      [(_ t ps o)
       #'(? Result? (app (lambda (result)
                           (match-define (Result: type propset optobject _) result)
                           (list type propset optobject))
                         (list t ps o)))]
      [(_ t ps o n)
       #'(? Result? (app (lambda (result)
                           (match-define (Result: type propset optobject n-existentials) result)
                           (list type propset optobject n-existentials))
                         (list t ps o n)))])))

(define-match-expander ExistentialResult:*
  (lambda (stx)
    (syntax-case stx ()
      [(_ t ps o n)
       #'(? Result? (app (lambda (result)
                           (match-define (Result: type propset optobject n-existentials) result)
                           (define syms (build-list n-existentials (lambda _ (gensym))))
                           (define vars (map make-F syms))
                           (list (instantiate-type type vars) (instantiate-propset propset vars) optobject vars))
                         (list t ps o
                               (? (lambda (l)
                                    (and (not (null? l)) l))
                                  n))))])))
