#lang racket/base

(require "../utils/utils.rkt"
         "rep-utils.rkt"
         "free-variance.rkt"
         "core-rep.rkt"
         (contract-req)
         racket/match
         syntax/parse/define
         racket/lazy-require)

(provide SomeValues?)
(provide-for-cond-contract Values/c)

;;**************************************************************
;; SomeValues (i.e. the things that can returned from functions)
;;**************************************************************

;;---------
;; Values
;;---------

(def-values Values ([results (listof Result?)])
  [#:intern-key (map Rep-seq results)]
  [#:frees (f) (combine-frees (map f results))]
  [#:fold (f) (make-Values (map f results))]
  [#:walk (f) (for-each f results)])

;; Anything that can be treated as a _simple_
;; Values by sufficient expansion
(define/provide (Values/c? x)
  (or (Type? x) (Values? x) (Result? x)))

(define-for-cond-contract Values/c (flat-named-contract 'Values Values/c?))

;;------------
;; AnyValues
;;------------


;; A Type that corresponds to the any contract for the
;; return type of functions

(def-values AnyValues ([p Prop?])
  [#:intern-key (Rep-seq p)]
  [#:frees (f) (f p)]
  [#:fold (f) (make-AnyValues (f p))]
  [#:walk (f) (f p)])

;;-------------
;; ValuesDots
;;-------------


(def-values ValuesDots ([results (listof Result?)]
                        [dty Type?]
                        [dbound (or/c symbol? natural-number/c)])
  [#:intern-key (list* (Rep-seq dty) dbound (map Rep-seq results))]
  [#:frees (f) (combine-frees (map f results))]
  [#:fold (f) (make-ValuesDots (map f results) (f dty) dbound)]
  [#:walk (f) (begin (f dty)
                     (for-each f results))])
