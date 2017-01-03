#lang racket/base

(require "rep-utils.rkt"
         "type-mask.rkt"
         "core-rep.rkt"
         "base-types.rkt"
         "numeric-base-types.rkt"
         racket/match
         (for-syntax racket/base))

(provide BaseUnion-bases:
         BaseUnion-bases)

(def-type BaseUnion ([bbits exact-nonnegative-integer?]
                     [nbits exact-nonnegative-integer?])
  #:base
  [#:mask (match-lambda [(BaseUnion: bbits nbits)
                         (cond
                           [(eqv? #b0 bbits) mask:number]
                           [(eqv? #b0 nbits) mask:base]
                           [else mask:base+number])])]
  [#:custom-constructor
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
