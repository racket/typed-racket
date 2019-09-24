#lang racket/base

(require (for-syntax "type-name-error.rkt"
                     racket/base))

(define-syntax (define-other-types stx)
  (syntax-case stx ()
    [(_ nm ...)
     #'(begin (define-syntax nm type-name-error)
              ...
              (provide nm) ...)]))

(define-syntax (define-other-props stx)
  (syntax-case stx ()
    [(_ nm ...)
     #'(begin (define-syntax nm prop-name-error)
              ...
              (provide nm) ...)]))

;; special type names that are not bound to particular types
(define-other-types
  -> ->* case-> U Union ∩ Intersection Rec All Opaque Immutable-Vector Mutable-Vector Vector
  Parameterof List List* Class Object Row Unit Values AnyValues Instance Refinement
  pred Struct Struct-Type Prefab PrefabTop Distinction Sequenceof Refine Self Imp Struct-Property Has-Struct-Property Exist)

(define-other-props
  Top Bot !)

(provide (rename-out [All ∀]
                     [U Un]
                     [-> →]
                     [case-> case→]
                     [List Tuple]
                     [Rec mu]
                     [Parameterof Parameter]))
