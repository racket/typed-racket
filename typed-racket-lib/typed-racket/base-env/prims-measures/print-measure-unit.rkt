#lang racket/base

(provide print-measure-unit measure-unit->sexp)

(require racket/match
         "../../rep/measure-unit-rep.rkt"
         )

(define (print-measure-unit type port write?)
  (display (measure-unit->sexp type) port))

(define (measure-unit->sexp u)
  (match u
    [(measure-unit: ht)
     `(u* ,@(for/list ([(k v) ht])
              (match-define (list nm id) k)
              `(u^ ,nm ,v)))]
    [(measure-unit/F: deps u) ; these will already be within an All type
     (measure-unit->sexp u)]
    ))
