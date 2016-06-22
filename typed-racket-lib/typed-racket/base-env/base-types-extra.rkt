#lang racket/base

(require (for-syntax "type-name-error.rkt"
                     racket/base syntax/stx))

(define-syntax (define-other-types stx)
  (syntax-case stx ()
    [(_ nm ...)
     #'(begin (define-syntax nm type-name-error)
              ...
              (provide nm) ...)]))

;; special type names that are not bound to particular types
(define-other-types
  -> ->* case-> U Rec All Opaque Vector
  Parameterof List List* Class Object Unit Values AnyValues Instance Refinement
  pred Struct Struct-Type Prefab Top Bot Distinction Sequenceof
  ∩)

(provide (rename-out [All ∀]
                     [U Un]
                     [-> →]
                     [case-> case→]
                     [List Tuple]
                     [Rec mu]
                     [Parameterof Parameter]))
