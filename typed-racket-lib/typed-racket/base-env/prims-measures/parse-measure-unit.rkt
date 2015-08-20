#lang racket/base

(provide parse-measure-unit)

(require syntax/parse
         syntax/stx
         racket/syntax
         "../../rep/measure-unit-rep.rkt"
         "../../env/measure-unit-env.rkt"
         (for-template "base-measure-unit.rkt")
         )

(define (parse-measure-unit stx)
  (syntax-parse stx #:literals (base-measure-unit)
    [(base-measure-unit nm:id id:id)
     (make-base-measure-unit (syntax-e #'nm) (syntax-e #'id))]
    [u:id
     (lookup-measure-unit #'u parse-measure-unit)]
    [n:integer
     (syntax-e #'n)]
    [(f:expr arg:expr ...)
     (define f* (parse-measure-unit #'f))
     (define args (stx-map parse-measure-unit #'(arg ...)))
     (apply f* args)]))
