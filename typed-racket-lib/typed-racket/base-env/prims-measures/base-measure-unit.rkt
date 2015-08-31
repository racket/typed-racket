#lang racket/base

(provide base-measure-unit
         def-measure-unit-stx-err
         )

(require (for-syntax racket/base
                     syntax/parse
                     racket/syntax
                     syntax/stx
                     ))

(begin-for-syntax
  (define/with-syntax stx-err-fun
    #'(lambda (stx)
        (raise-syntax-error
         #f
         "measure-unit name used out of context"
         stx
         (and (stx-pair? stx) (stx-car stx))))))

(define-syntax def-measure-unit-stx-err
  (syntax-parser
    [(def id:id ...)
     #'(begin
         (define-syntax id stx-err-fun)
         ...)]))

(def-measure-unit-stx-err base-measure-unit)

