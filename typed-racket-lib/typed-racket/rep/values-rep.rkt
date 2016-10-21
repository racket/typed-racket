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
  [#:frees (f) (combine-frees (map f results))]
  [#:fmap (f) (make-Values (map f results))]
  [#:for-each (f) (for-each f results)])

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
  [#:frees (f) (f p)]
  [#:fmap (f) (make-AnyValues (f p))]
  [#:for-each (f) (f p)])

;;-------------
;; ValuesDots
;;-------------


(def-values ValuesDots ([results (listof Result?)]
                        [dty Type?]
                        [dbound (or/c symbol? natural-number/c)])
  [#:frees
   [#:vars (f)
    (if (symbol? dbound)
        (free-vars-remove (combine-frees (map free-vars* (cons dty results))) dbound)
        (combine-frees (map free-vars* (cons dty results))))]
   [#:idxs (f)
    (if (symbol? dbound)
        (combine-frees (cons (single-free-var dbound) 
                             (map free-idxs* (cons dty results))))
        (combine-frees (map free-idxs* (cons dty results))))]]
  [#:fmap (f) (make-ValuesDots (map f results) (f dty) dbound)]
  [#:for-each (f) (begin (f dty)
                         (for-each f results))])
