#lang racket/base

;; This module provides unsafe operations for Typed Racket

(provide provide/unsafe
         require/typed/unsafe)

(require (for-syntax racket/base
                     typed-racket/private/syntax-properties
                     (submod typed-racket/base-env/prims-contract unsafe)))

(define-syntax (require/typed/unsafe stx)
  (-require/typed/unsafe stx))

(define-syntax (provide/unsafe stx)
  (syntax-case stx ()
    [(_ . rst)
     (quasisyntax/loc stx
       #,(unsafe-provide #'(provide . rst)))]))
