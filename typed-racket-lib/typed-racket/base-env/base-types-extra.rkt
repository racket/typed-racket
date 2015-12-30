#lang racket/base

(require (for-syntax racket/base syntax/stx))

(define-syntax (define-other-types stx)
  (syntax-case stx ()
    [(_ nm ...)
     #'(begin (define-syntax nm
                (lambda (stx)
                  (raise-syntax-error 'type-check
                                      (format "type name used out of context\n  type: ~a\n  in: ~a"
                                              (syntax->datum (if (stx-pair? stx)
                                                                 (stx-car stx)
                                                                 stx))
                                              (syntax->datum stx))
                                      stx
                                      (and (stx-pair? stx) (stx-car stx)))))
              ...
              (provide nm) ...)]))

;; special type names that are not bound to particular types
(define-other-types
  -> ->* case-> U Rec All Opaque Vector
  Parameterof List List* Class Object Unit Values AnyValues Instance Refinement
  pred Struct Struct-Type Prefab Top Bot Distinction Sequenceof)

(provide (rename-out [All ∀]
                     [U Un]
                     [-> →]
                     [case-> case→]
                     [List Tuple]
                     [Rec mu]
                     [Parameterof Parameter]))
