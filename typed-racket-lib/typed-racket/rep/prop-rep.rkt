#lang racket/base

(require "../utils/utils.rkt" "rep-utils.rkt" "free-variance.rkt")

(provide hash-name prop-equal?)

(begin-for-cond-contract
  (require racket/contract/base racket/lazy-require)
  (lazy-require ["type-rep.rkt" (Type/c Univ? Bottom?)]
                ["object-rep.rkt" (Path?)]))

(provide-for-cond-contract name-ref/c)


;; A Name-Ref is any value that represents an object.
;; As an identifier, it represents a free variable in the environment
;; As a list, it represents a De Bruijn indexed bound variable
(define-for-cond-contract name-ref/c
  (or/c identifier? (list/c integer? integer?)))
(define (hash-name v) (if (identifier? v) (hash-id v) (list v)))

(define-for-cond-contract ((length>=/c len) l)
  (and (list? l)
       (>= (length l) len)))

;; the trivially "true" proposition
(def-prop TrueProp () [#:fold-rhs #:base])
;; the absurd, "false" proposition
(def-prop FalseProp () [#:fold-rhs #:base])

(def-prop TypeProp ([p Path?] [t (and/c Type/c (not/c Univ?) (not/c Bottom?))])
  [#:intern (list (Rep-seq t) (Rep-seq p))]
  [#:frees (λ (f) (combine-frees (map f (list t p))))]
  [#:fold-rhs (*TypeProp (object-rec-id p) (type-rec-id t))])

(def-prop NotTypeProp ([p Path?] [t (and/c Type/c (not/c Univ?) (not/c Bottom?))])
  [#:intern (list (Rep-seq t) (Rep-seq p))]
  [#:frees (λ (f) (combine-frees (map f (list t p))))]
  [#:fold-rhs (*NotTypeProp (object-rec-id p) (type-rec-id t))])

(def-prop OrProp ([fs (and/c (length>=/c 2)
                           (listof (or/c TypeProp? NotTypeProp?)))])
  [#:intern (map Rep-seq fs)]
  [#:fold-rhs (*OrProp (map prop-rec-id fs))]
  [#:frees (λ (f) (combine-frees (map f fs)))])

(def-prop AndProp ([fs (and/c (length>=/c 2)
                          (listof (or/c OrProp? TypeProp? NotTypeProp?)))])
  [#:intern (map Rep-seq fs)]
  [#:fold-rhs (*AndProp (map prop-rec-id fs))]
  [#:frees (λ (f) (combine-frees (map f fs)))])

(def-prop PropSet ([thn Prop?] [els Prop?])
  [#:fold-rhs (*PropSet (prop-rec-id thn) (prop-rec-id els))])

(define (prop-equal? a b) (= (Rep-seq a) (Rep-seq b)))
