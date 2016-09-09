#lang racket/base

;;************************************************************
;; Type Masks
;;
;; - - Purpose - -
;; 
;; Type masks allow us to identify disjoint base types and unions of
;; base types. This allows us to short-circuit certain subtype and
;; overlap checks.
;;
;; - - Details - -
;;
;; Type masks are represented with a simple 31-bit fixnum.
;;
;; If a bit flag in a Type's bitmask is set to 1, it means the Type
;; _may_ overlap with the values described by that bit flag.
;;
;; If a bit flag in a Type's bitmask is set to 0, it means the Type
;; _cannot_ overlap with values described by that bit flag.
;;************************************************************

(require (for-syntax racket/base syntax/parse)
         racket/unsafe/ops
         racket/fixnum)

(provide type-mask?
         mask-union
         mask-intersect
         disjoint-masks?
         sub-mask?
         mask:bottom
         mask:unknown)

(define-syntax OR (make-rename-transformer #'unsafe-fxior))
(define-syntax AND (make-rename-transformer #'unsafe-fxand))
(define-syntax NOT (make-rename-transformer #'unsafe-fxnot))
(define-syntax EQUALS? (make-rename-transformer #'unsafe-fx=))
(define-syntax mask-union (make-rename-transformer #'unsafe-fxior))
(define-syntax mask-intersect (make-rename-transformer #'unsafe-fxand))

;; debugging safe versions
;; (define-syntax OR (make-rename-transformer #'fxior))
;; (define-syntax AND (make-rename-transformer #'fxand))
;; (define-syntax NOT (make-rename-transformer #'fxnot))
;; (define-syntax EQUALS? (make-rename-transformer #'fx=))
;; (define-syntax mask-union (make-rename-transformer #'fxior))
;; (define-syntax mask-intersect (make-rename-transformer #'fxand))

;; type mask predicate
(define-syntax type-mask? (make-rename-transformer #'fixnum?))

;; define the max size of type  masks
(module const racket/base
  (provide max-mask-size)
  (define max-mask-size 31))
(require 'const (for-syntax 'const))


;;************************************************************
;; Mask Operations
;;************************************************************

(define-syntax-rule (ZERO? n)
  (EQUALS? 0 n))

;; disjoint-masks?
;; returns #t if the two masks could not
;; possibly have overlapping values
(define (disjoint-masks? m1 m2)
  (ZERO? (mask-intersect m1 m2)))

;; sub-mask?
;; returns #t if it is possible that m1 ⊆ m2
;; (i.e. values represented by m1 are also
;;  described by m2)
(define (sub-mask? m1 m2)
  (ZERO? (AND m1 (NOT m2))))


;;************************************************************
;; Masks
;;************************************************************

;;---------------------
;; declare-type-flags
;;---------------------

;; macro for easily defining the type mask flags
(define-syntax (declare-type-flags stx)
  (syntax-parse stx
    [(_ name:id ...)
     (define name-list (syntax->datum #'(name ...)))
     (define count (length name-list))
     (unless (<= count max-mask-size)
       (raise-syntax-error 'declare-type-flags
                           (format "too many type flags (~a is the max)"
                                   max-mask-size)
                           stx))
     (with-syntax ([(n ...) (build-list count (λ (n) (arithmetic-shift 1 n)))])
       #`(begin (begin (define name n)
                       (provide name))
                ...))]))

;;-------------------
;; Top/Bottom Masks
;;-------------------

;; bottom mask - no value inhabits this mask
(define mask:bottom 0)
;; unknown/top mask - this mask says the value may inhabit any type
(define mask:unknown
  (sub1 (expt 2 max-mask-size)))

;;----------------------
;; Specific Type Flags
;;----------------------

;; Note:  mask:other is for values which are
;; disjoint from all other specified values,
;; but which we are not specifically tracking

(declare-type-flags
 ;; a few common base types have their own masks
 mask:null
 mask:true
 mask:false
 mask:char
 mask:symbol
 mask:void
 mask:string
 ;; the other base types use this catch-all
 mask:base-other
 mask:number
 mask:pair
 mask:mpair
 mask:vector
 mask:hash
 mask:box
 mask:channel
 mask:thread-cell
 mask:promise ;; huh? (structs can be promises)
 mask:ephemeron
 mask:future
 mask:other-box
 mask:set
 mask:procedure
 mask:prompt-tag
 mask:continuation-mark-key
 mask:struct
 mask:prefab
 mask:struct-type
 mask:syntax
 mask:class
 mask:instance
 mask:unit)
