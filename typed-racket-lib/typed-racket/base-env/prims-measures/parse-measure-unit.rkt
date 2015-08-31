#lang racket/base

(provide parse-measure-unit)

(require syntax/parse
         syntax/stx
         racket/syntax
         racket/list
         racket/match
         unstable/match
         "../../rep/measure-unit-rep.rkt"
         "../../rep/type-rep.rkt"
         "../../env/measure-unit-env.rkt"
         "../../env/tvar-env.rkt"
         (for-template "base-measure-unit.rkt")
         )

(define (parse-measure-unit stx)
  (syntax-parse stx #:literals (base-measure-unit)
    [(base-measure-unit nm:id id:id)
     (make-base-measure-unit (syntax-e #'nm) (syntax-e #'id))]
    [u:id
     (cond
       ;; if it's a type variable, we take the corresponding reference in the HT
       ;;   and put it in an F-measure-unit
       [(bound-tvar? (syntax-e #'u))
        (define tvar (lookup-tvar (syntax-e #'u)))
        (match tvar
          [(F: (? symbol? sym))
           (when (not (symbol-interned? sym))
             (printf "warning: parse-measure-unit.rkt: uninterned symbol: ~v, from: ~v\n" sym #'u))
           (make-F-measure-unit sym)])]
       [else
        (lookup-measure-unit #'u parse-measure-unit)])]
    [n:integer
     (syntax-e #'n)]
    [(f:expr arg:expr ...)
     (define f* (parse-measure-unit #'f))
     (define args (stx-map parse-measure-unit #'(arg ...)))
     (apply/F-measure-units f* args)]))
