#lang racket
(require
 "utils.rkt"
 (rep type-rep prop-rep values-rep))
(provide (all-defined-out))

;; dom-info is a (dom-info Option<Syntax> Option<Deps> Syntax ArgType Boolean)
;; Note: its components contain unexpanded/surface syntax.
;; -- `id' the syntax of the id the user wrote or false if this domain is unnamed
;; -- `deps' is either a possibly empty Listof<Syntax> for the list of
;;    identifiers this domain depends on or false if there are none (which is
;;    different from writing an empty dependency list)
;; -- `ctc' is the syntax the user wrote for the contract
;; -- `type' is either a natural number or a keyword. If it's a natural, the
;;    domain is a by-position domain, and the number indicates its relative
;;    position in the function's domain. If it's a keyword, the domain is a
;;    keyword arg and the keyword is what the user wrote for this domain. Must be
;;    unique amongst all other dom-info instances for a particular contract
;; -- `mandatory?' is #t if the domain is mandatory, #f otherwise
(struct dom-info (id deps ctc type mandatory?) #:transparent)

;; pre/post-info is a (pre/post-info Option<Deps> Natural Boolean)
;; Note: its components contain unexpanded/surface syntax.
;; -- `deps' is either a possible empty Listof<Syntax> for the list of
;;    identifiers this condition depends on or false if there are none
;; -- `position' is the position of this condition relative to other conditions
;; -- `desc?' is true if the condition is a /desc variant; false otherwise
(struct pre-info (deps position desc?) #:transparent)
(struct post-info (deps position desc?) #:transparent)

;; rng-info is a (rng-info Option<Syntax> Option<Deps> Syntax Natural)
;; Note: its components contain unexpanded/surface syntax.
;; -- `id' the syntax of the id the user wrote or false if this range is unnamed
;; -- `deps' is either a possibly empty Listof<Syntax> for the list of
;;    identifiers this range depends on or false if there are none (which is
;;    different from writing an empty dependency list)
;; -- `ctc' is the syntax the user wrote for the contract, possibly even 'any' in
;;    the case of an any dependent-range
;; -- `index' is the position of the range in a possibly multi-valued range. For
;;    a non-(values ...) range, then this will simply be 0
(struct rng-info (id deps ctc index) #:transparent)

(struct rest-info (id deps ctc) #:transparent)

(define (Con*-in-ty ctc) (match ctc [(Con*: in out) in]))
(define (Con*-out-ty ctc) (match ctc [(Con*: in out) out]))
(define-match-expander Con*:
  (lambda (stx)
    (syntax-case stx ()
      [(_ in out)
       #'(or
          (? FlatCon? (app FlatCon-in-ty in) (app FlatCon-out-ty out))
          (? Con? (app Con-in-ty in) (app Con-out-ty out)))])))

;; A ConFnInfo is a (List Type? Type?) representing the in/out type components
;; of a function that could be coerced to a FlatCon

;; con-fn-in : Type? -> Type?
;; Assumes ty has non-#f confn-type-components
(define (confn-in ty) (car (confn-type-components ty)))
;; confn-out : Type? -> Type?
;; Assumes ty has non-#f confn-type-components
(define (confn-out ty) (cadr (confn-type-components ty)))
;; confn-type-components : Type? -> #f or ConFnInfo
;; Note: only gets components for functions with a single unary arrow
(define (confn-type-components ty)
  ;; TODO: find all unary arities and union their inputs/meet their outputs
  (match ty
    [(Fun: (list (Arrow:
                  (list in-ty)
                  _ _
                  (Values: (list (Result: _ (PropSet: (TypeProp: _ out-ty) _) _))))))
     (list in-ty out-ty)]
    [(Fun: (list (Arrow: (list in-ty) _ _ _)))
     ;; when there isn't a TypeProp
     (list in-ty in-ty)]
    [_ #f]))
(define-match-expander ConFn*:
  (lambda (stx)
    (syntax-case stx ()
      [(_ in out)
       #'(? confn-type-components (app confn-in in) (app confn-out out))])))

;; Various symbols used as keys in identifying pieces of contract expansion
(define and/c-key 'and/c)
(define and/c-index-key 'and/c-index)

(define or/c-key 'or/c)
(define or/c-index-key 'or/c-index)

(define list/c-key 'list/c)
(define list/c-index-key 'list/c-index)

(define ->-key '->)
(define ->-dom-key '->-dom)
(define ->-rng-key '->-rng)

(define ->i-key '->i)
(define ->i-dom-key '->i-dom)
(define ->i-rng-key '->i-rng)
(define ->i-rest-key '->i-rest)
(define ->i-pre-key '->i-pre)
(define ->i-post-key '->i-post)


