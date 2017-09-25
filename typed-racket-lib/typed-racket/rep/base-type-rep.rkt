#lang racket/base

(require "rep-utils.rkt"
         "core-rep.rkt"
         "type-mask.rkt"
         racket/match
         (for-syntax racket/base
                     racket/syntax
                     syntax/parse))

(provide define-base-types
         Base?
         Base-name
         Base-predicate
         Base:
         Base-bits:
         Base-predicate:
         Base-name/contract:
         Base-bits)

;;-----------------
;; Base Type
;;-----------------

;; name - a printable type name (unique for each Base)
;; numeric? - is this Base type for values where 'number?' is #t
;; bits - the binary representation of this Base type, used in BaseUnions
;; to efficiently represent unions of Bases. NOTE: this should include only
;; one bit set to 1.
;; contract - used when generating contracts from types
;; predicate - used to check (at compile-time) whether a value belongs
;; to that base type. This is used to check for subtyping between value
;; types and base types.
(def-type Base ([name symbol?]
                [numeric? boolean?]
                [bits exact-nonnegative-integer?]
                [contract syntax?]
                [predicate procedure?])
  #:base
  #:no-provide
  [#:mask (λ (t) (if (Base-numeric? t)
                     mask:number
                     mask:base))]
  #:non-transparent
  [#:extras
   ;; equality only need consider the 'name' field
   #:methods gen:equal+hash
   [(define (equal-proc a b _)
      (eq? (Base-name a) (Base-name b)))
    (define (hash-proc a _)
      (equal-hash-code (Base-name a)))
    (define (hash2-proc a _)
      (equal-secondary-hash-code (Base-name a)))]])

(define-match-expander Base-bits:
  (λ (stx) (syntax-case stx ()
             [(_ numeric? bits)
              (syntax/loc stx (Base: _ numeric? bits _ _))])))

(define-match-expander Base-predicate:
  (λ (stx) (syntax-case stx ()
             [(_ pred)
              (syntax/loc stx (Base: _ _ _ _ pred))])))

(define-match-expander Base-name/contract:
  (λ (stx) (syntax-case stx ()
             [(_ name ctc)
              (syntax/loc stx (Base: name _ _ ctc _))])))

;; macro for easily defining sets of types whose representation
;; relies on predefined fixnum bitfields
(define-syntax (define-base-types stx)
  (define-syntax-class atoms-spec
    (pattern [abbrev:id
              name:id
              contract:expr
              predicate:expr]
             #:with type-pred (format-id #'name "Base:~a?" (syntax-e #'name))
             #:with provide #'(provide abbrev type-pred)))
  (syntax-parse stx
    [(_ #:numeric? num?:boolean
        #:max-count max-count-stx:exact-nonnegative-integer
        #:count count:id
        #:atom-vector atom-vector:id
        #:atom-hash atom-hash:id
        #:atoms
        atoms:atoms-spec ...)
     (define max-count (syntax->datum #'max-count-stx))
     (define atom-list (syntax->datum #'(atoms.name ...)))
     (define atom-count (length atom-list))
     (unless (<= atom-count max-count)
       (raise-syntax-error
        'define-type-bitfield
        (format "too many atomic base types (~a is the max)"
                max-count)
        stx))
     (with-syntax ([(n ... ) (for/list ([i (in-range atom-count)]) i)]
                   [(2^n ...)
                    (build-list atom-count (λ (n) (arithmetic-shift 1 n)))])
       #`(begin
           (define count #,atom-count)
           ;; define the actual type references (e.g. -Null)
           (define/decl atoms.abbrev
             (make-Base (quote atoms.name) num? 2^n atoms.contract atoms.predicate))
           ...
           (define atoms.type-pred (λ (t) (equal? t atoms.abbrev))) ...
           atoms.provide ...
           (define atom-vector
             (vector-immutable atoms.abbrev ...))
           (define atom-hash
             (make-immutable-hasheqv (list (cons 2^n atoms.abbrev) ...)))))]))
