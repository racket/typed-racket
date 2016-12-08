#lang racket/base

(require "../utils/utils.rkt"
         (contract-req)
         "rep-utils.rkt"
         "free-variance.rkt"
         "core-rep.rkt"
         "object-rep.rkt"
         racket/match
         racket/lazy-require)

(lazy-require
 ["../types/prop-ops.rkt" (-and -or)])

(provide -is-type
         -not-type)


(def-prop TypeProp ([obj Object?] [type (and/c Type? (not/c Univ?) (not/c Bottom?))])
  [#:frees (f) (combine-frees (list (f obj) (f type)))]
  [#:fmap (f) (make-TypeProp (f obj) (f type))]
  [#:for-each (f) (begin (f obj) (f type))]
  [#:custom-constructor
   (cond
     [(Empty? obj) -tt]
     [(Univ? type) -tt]
     [(Bottom? type) -ff]
     [else (make-TypeProp obj type)])])

;; Abbreviation for props
;; `i` can be an integer or name-ref/c for backwards compatibility
;; FIXME: Make all callers pass in an object and remove backwards compatibility
(define/cond-contract (-is-type i t)
  (-> (or/c integer? name-ref/c OptObject?) Type? Prop?)
  (define o
    (cond
      [(OptObject? i) i]
      [(exact-integer? i) (make-Path null (cons 0 i))]
      [(pair? i) (make-Path null i)]
      [else (-id-path i)]))
  (make-TypeProp o t))

(def-prop NotTypeProp ([obj Object?] [type (and/c Type? (not/c Univ?) (not/c Bottom?))])
  [#:frees (f) (combine-frees (list (f obj) (f type)))]
  [#:fmap (f) (-not-type (f obj) (f type))]
  [#:for-each (f) (begin (f obj) (f type))]
  [#:custom-constructor
   (cond
     [(Empty? obj) -tt]
     [(Univ? type) -ff]
     [(Bottom? type) -tt]
     [else (make-NotTypeProp obj type)])])


;; Abbreviation for not props
;; `i` can be an integer or name-ref/c for backwards compatibility
;; FIXME: Make all callers pass in an object and remove backwards compatibility
(define/cond-contract (-not-type i t)
  (-> (or/c integer? name-ref/c OptObject?) Type? Prop?)
  (define o
    (cond
      [(OptObject? i) i]
      [(exact-integer? i) (make-Path null (cons 0 i))]
      [(pair? i) (make-Path null i)]
      [else (-id-path i)]))
  (make-NotTypeProp o t))

(def-prop OrProp ([ps (listof (or/c TypeProp? NotTypeProp?))])
  [#:frees (f) (combine-frees (map f ps))]
  [#:fmap (f) (apply -or (map f ps))]
  [#:for-each (f) (for-each f ps)])

(def-prop AndProp ([ps (listof (or/c OrProp? TypeProp? NotTypeProp?))])
  [#:frees (f) (combine-frees (map f ps))]
  [#:fmap (f) (apply -and (map f ps))]
  [#:for-each (f) (for-each f ps)])
