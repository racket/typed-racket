#;
(exn-pred "3 contract violation")
#lang racket/base

(module u racket/base
  (struct apple (a))
  (struct pear (a) #:constructor-name make-pear)
  (provide (struct-out apple)
           (struct-out pear)))

(module t typed/racket
  (require/typed/provide (submod ".." u)
    (#:struct apple ((a : Symbol)))
    (#:struct pear ((a : Number)) #:constructor-name make-pear)))

(define counter 0)
(require 't)

(define-syntax-rule (verify-contract expr ...)
  (begin (with-handlers ([exn:fail:contract? (lambda _
                                               (set! counter (add1 counter)))])
           expr)
         ...
         (error (format "~a contract violation(s)" counter))))

(verify-contract (apple 42)
                 (apple-a 20)
                 (make-pear 'xxx))

