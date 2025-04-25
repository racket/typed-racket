#lang racket/base

(provide
  plambda-prop
  has-poly-annotation?
  get-poly-layer
  remove-poly-layer
  get-poly-tvarss)

(require
  racket/list
  racket/match
  typed-racket/env/scoped-tvar-env
  typed-racket/private/syntax-properties)


(define (plambda-prop stx)
  (define d (plambda-property stx))
  (and d (car (flatten d))))

(define (has-poly-annotation? form)
  (or (plambda-prop form) (pair? (lookup-scoped-tvar-layer form))))

(define (get-poly-layer tvarss)
  (map car tvarss))

(define (remove-poly-layer tvarss)
  (filter pair? (map rest tvarss)))

(define (get-poly-tvarss form)
  (define plambda-tvars
    (let ([p (plambda-prop form)])
      (match (and p (map syntax-e (syntax->list p)))
        [#f #f]
        [(list var ... dvar '...) (list (list var dvar))]
        [(list id ...) (list id)])))
  (define scoped-tvarss
    (for/list ([tvarss (in-list (lookup-scoped-tvar-layer form))])
      (for/list ([tvar (in-list tvarss)])
        (match tvar
          [(list (list v ...) dotted-v) (list (map syntax-e v) (syntax-e dotted-v))]
          [(list v ...) (map syntax-e v)]))))
  (if plambda-tvars
      (cons plambda-tvars scoped-tvarss)
      scoped-tvarss))

