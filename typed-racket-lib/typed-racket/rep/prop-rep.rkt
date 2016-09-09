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

(provide hash-name
         -is-type
         -not-type
         AndProp?
         AndProp:
         AndProp-ps
         OrProp?
         OrProp:
         OrProp-ps
         (rename-out [make-OrProp* make-OrProp]
                     [make-AndProp* make-AndProp]))


(def-prop TypeProp ([obj Object?] [type (and/c Type? (not/c Univ?) (not/c Bottom?))])
  [#:intern-key (cons (Rep-seq obj) (Rep-seq type))]
  [#:frees (f) (combine-frees (list (f obj) (f type)))]
  [#:fold (f) (-is-type (f obj) (f type))]
  [#:walk (f) (begin (f obj) (f type))])

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
  (cond
    [(Empty? o) (make-TrueProp)]
    [(Univ? t) (make-TrueProp)]
    [(Bottom? t) (make-FalseProp)]
    [else (make-TypeProp o t)]))

(def-prop NotTypeProp ([obj Object?] [type (and/c Type? (not/c Univ?) (not/c Bottom?))])
  [#:intern-key (cons (Rep-seq obj) (Rep-seq type))]
  [#:frees (f) (combine-frees (list (f obj) (f type)))]
  [#:fold (f) (-not-type (f obj) (f type))]
  [#:walk (f) (begin (f obj) (f type))])


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
  (cond
    [(Empty? o) (make-TrueProp)]
    [(Bottom? t) (make-TrueProp)]
    [(Univ? t) (make-FalseProp)]
    [else (make-NotTypeProp o t)]))

(def-prop OrProp ([ps (and/c (length>=/c 2)
                             (listof (or/c TypeProp? NotTypeProp?)))])
  #:no-provide
  [#:intern-key (for/hash ([p (in-list ps)]) (values p #t))]
  [#:frees (f) (combine-frees (map f ps))]
  [#:fold (f) (apply -or (map f ps))]
  [#:walk (f) (for-each f ps)])

(define (make-OrProp* ps)
  (match ps
    [(list) (make-FalseProp)]
    [(list p) p]
    [ps (make-OrProp ps)]))

(def-prop AndProp ([ps (and/c (length>=/c 2)
                              (listof (or/c OrProp? TypeProp? NotTypeProp?)))])
  #:no-provide
  [#:intern-key (for/hash ([p (in-list ps)]) (values p #t))]
  [#:frees (f) (combine-frees (map f ps))]
  [#:fold (f) (apply -and (map f ps))]
  [#:walk (f) (for-each f ps)])

(define (make-AndProp* ps)
  (match ps
    [(list) (make-TrueProp)]
    [(list p) p]
    [ps (make-AndProp ps)]))
