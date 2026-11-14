#lang racket/base

;; Helpers for operations that follow the variance of type constructors,
;; such as substituting objects into types (typecheck/tc-subst.rkt) and
;; eliminating type variables (infer/promote-demote.rkt). When such an
;; operation cannot change an invariant part of a type, it replaces the
;; whole type with its top type (in a positive position) or with Bottom
;; (in a negative one).

(require "../utils/prefab.rkt"
         racket/match
         "../env/type-constr-env.rkt"
         "../rep/type-rep.rkt"
         "../rep/type-constr.rkt")

(provide top-of
         invariant-type?
         app-variances)

;; top-of : Type -> Type
;; the least supertype of `t` whose invariant parts are not fixed
(define (top-of t)
  (match t
    [(? MPair?) -MPairTop]
    [(or (? Mutable-Vector?) (? Mutable-HeterogeneousVector?)) -Mutable-VectorTop]
    [(? Box?) -BoxTop]
    [(? Channel?) -ChannelTop]
    [(? Async-Channel?) -Async-ChannelTop]
    [(? ThreadCell?) -ThreadCellTop]
    [(? Weak-Box?) -Weak-BoxTop]
    [(? Mutable-HashTable?) -Mutable-HashTableTop]
    [(? Weak-HashTable?) -Weak-HashTableTop]
    [(? Prompt-Tagof?) -Prompt-TagTop]
    [(? Continuation-Mark-Keyof?) -Continuation-Mark-KeyTop]
    [(Prefab: key _) (make-PrefabTop key)]
    [(? Class?) -ClassTop]
    [(? Unit?) -UnitTop]
    [(? StructType?) -StructTypeTop]
    [_ Univ]))

;; invariant-type? : Type -> Boolean
;; does `t` have invariant parts that its Rep-variances do not describe?
(define (invariant-type? t)
  (or (and (Struct? t) (ormap fld-mutable? (Struct-flds t)))
      (Mutable-HeterogeneousVector? t)
      (and (Prefab? t) (prefab-key/mutable-fields? (Prefab-key t)))
      ;; these mix positive, negative, and mutable parts
      (Class? t)
      (Instance? t)
      (Unit? t)
      (StructType? t)
      (Struct-Property? t)))

;; app-variances : Type (Listof Type) -> (U (Listof Variance) #f)
;; the variances of the arguments of an applied type constructor, if known
(define (app-variances rator rands)
  (match rator
    [(Name: id _ _)
     (match (lookup-type-constructor id)
       [(struct* TypeConstructor ([variances (? list? variances)]))
        #:when (= (length variances) (length rands))
        variances]
       [_ #f])]
    [_ #f]))
