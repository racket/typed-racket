#lang racket/base

(require "../utils/utils.rkt"
         "rep-utils.rkt"
         "type-mask.rkt"
         "core-rep.rkt"
         "base-types.rkt"
         "numeric-base-types.rkt"
         racket/match
         (contract-req)
         (for-syntax racket/base))

(provide BaseUnion-bases:
         BaseUnion-bases)

;; BaseUnion
;;
;; BaseUnions contain a compact representation for unions
;; of Base types. Base types are divided into two categories:
;; those that are numeric (i.e. number? returns #t) and those
;; that are not. See 'base-types.rkt' and 'numeric-base-types.rkt'
;; for the various defined Base types.
;; 
;; bbits - the combined bits (via inclusive bit or) for
;; all Base members where Base-numeric? is #f
;; nbits - the combined bits (via inclusive bit or) for
;; all Base members where Base-numeric? is #t
(def-type BaseUnion ([bbits exact-nonnegative-integer?]
                     [nbits exact-nonnegative-integer?])
  #:base
  [#:mask (match-lambda [(BaseUnion: bbits nbits)
                         (cond
                           [(eqv? #b0 bbits) mask:number]
                           [(eqv? #b0 nbits) mask:base]
                           [else mask:base+number])])]
  [#:custom-constructor/contract
   (-> exact-nonnegative-integer?
       exact-nonnegative-integer?
       Type?)
   ;; make sure we do not build BaseUnions equivalent to
   ;; Bottom or a *single* Base type
   (cond
     [(eqv? bbits 0)
      (cond
        [(eqv? nbits 0) -Bottom]
        [(nbits->atom? nbits)]
        [else (make-BaseUnion bbits nbits)])]
     [(eqv? nbits 0)
      (cond
        [(bbits->atom? bbits)]
        [else (make-BaseUnion bbits nbits)])]
     [else (make-BaseUnion bbits nbits)])])

(define-match-expander BaseUnion-bases:
  (λ (stx) (syntax-case stx ()
             [(_ bases)
              (syntax/loc stx
                (and (? BaseUnion?)
                     (app BaseUnion-bases bases)))])))

(define (BaseUnion-bases t)
  (match t
    [(BaseUnion: bbits nbits)
     (cond
       [(eqv? bbits 0) (nbits->base-types nbits)]
       [(eqv? nbits 0) (bbits->base-types bbits)]
       [else (append (bbits->base-types bbits)
                     (nbits->base-types nbits))])]))
