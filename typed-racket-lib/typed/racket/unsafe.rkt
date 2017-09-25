#lang racket/base

;; This module provides unsafe operations for Typed Racket

(provide (protect-out unsafe-provide
                      unsafe-require/typed
                      unsafe-require/typed/provide))

(require (for-syntax racket/base
                     syntax/parse
                     (prefix-in internal: typed-racket/private/syntax-properties)
                     (prefix-in internal:  (submod typed-racket/base-env/prims-contract unsafe))))

(define-syntax (unsafe-require/typed stx)
  (internal:unsafe-require/typed stx))

(define-syntax (unsafe-provide stx)
  (quasisyntax/loc stx
    #,(internal:unsafe-provide #`(provide . #,stx))))

(define-syntax (unsafe-require/typed/provide stx)
  (unless (memq (syntax-local-context) '(module module-begin))
    (raise-syntax-error 'unsafe-require/typed/provide
                        "can only be used at module top-level"))
  (syntax-parse stx
    [(_ lib) #'(begin)]
    [(_ lib [r:id t] other-clause ...)
     #`(begin (unsafe-require/typed lib [r t])
              (provide r)
              (unsafe-require/typed/provide lib other-clause ...))]
    [(_ lib (~and clause [#:struct name:id ([f:id (~datum :) t] ...)
                          option ...])
        other-clause ...)
     #'(begin (unsafe-require/typed lib clause)
              (provide (struct-out name))
              (unsafe-require/typed/provide lib other-clause ...))]
    [(_ lib (~and clause [#:struct (name:id parent:id)
                          ([f:id (~datum :) t] ...)
                          option ...])
        other-clause ...)
     #'(begin (unsafe-require/typed lib clause)
              (provide (struct-out name))
              (unsafe-require/typed/provide lib other-clause ...))]
    [(_ lib (~and clause [#:opaque t:id pred:id])
        other-clause ...)
     #'(begin (unsafe-require/typed lib clause)
              (provide t pred)
              (unsafe-require/typed/provide lib other-clause ...))]))