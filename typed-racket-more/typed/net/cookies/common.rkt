#lang typed/racket/base

(require (for-syntax racket/base))

(define-syntax require/typed/provide-string-predicates
  (syntax-rules ()
    [(_ id)
     (begin (: id (-> Any Boolean : #:+ String))
            (provide id)
            (require/typed
             net/cookies/common
             [(id untyped-id) (-> Any Boolean)])
            (define (id v)
              (and (string? v)
                   (untyped-id v))))]
    [(_ id ...)
     (begin (require/typed/provide-string-predicates id) ...)]))

(require/typed/provide-string-predicates
 cookie-name?
 cookie-value?
 path/extension-value?
 domain-value?)
