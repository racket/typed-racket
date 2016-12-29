#lang racket/base

(require "rep-utils.rkt"
         "type-mask.rkt"
         "core-rep.rkt"
         "base-types.rkt"
         "numeric-base-types.rkt"
         racket/match
         (for-syntax racket/base))

(provide BaseUnion-bases:
         BaseUnion-bases
         BaseUnion?
         (rename-out [make-BaseUnion* make-BaseUnion]
                     [BaseUnion:* BaseUnion:]))

(def-type BaseUnion ([bits (cons/c exact-nonnegative-integer?
                                   exact-nonnegative-integer?)])
  #:base
  #:non-transparent
  #:no-provide
  [#:mask (match-lambda [(BaseUnion: (cons bbits nbits))
                         (cond
                           [(eqv? #b0 bbits) mask:number]
                           [(eqv? #b0 nbits) mask:base]
                           [else mask:base+number])])])
(define base-union-table (make-weak-hash))

(define (make-BaseUnion* bbits nbits)
  (define (make)
    (define bits (cons bbits nbits))
    (intern-single-ref! base-union-table
                        bits
                        #:construct (make-BaseUnion bits)))
  (cond
    [(eqv? bbits 0)
     (cond
       [(eqv? nbits 0) -Bottom]
       [(nbits->atom? nbits)]
       [else (make)])]
    [(eqv? nbits 0)
     (cond
       [(bbits->atom? bbits)]
       [else (make)])]
    [else (make)]))


(define-match-expander BaseUnion:*
  (λ (stx) (syntax-case stx ()
             [(_ bbits nbits)
              (syntax/loc stx
                (BaseUnion: (cons bbits nbits)))])))

(define-match-expander BaseUnion-bases:
  (λ (stx) (syntax-case stx ()
             [(_ bases)
              (syntax/loc stx
                (and (? BaseUnion?)
                     (app BaseUnion-bases bases)))])))

(define (BaseUnion-bases t)
  (match t
    [(BaseUnion: (cons bbits nbits))
     (cond
       [(eqv? bbits 0) (nbits->base-types nbits)]
       [(eqv? nbits 0) (bbits->base-types bbits)]
       [else (append (bbits->base-types bbits)
                     (nbits->base-types nbits))])]))
