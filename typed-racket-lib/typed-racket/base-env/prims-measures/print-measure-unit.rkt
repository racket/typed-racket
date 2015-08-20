#lang racket/base

(provide print-measure-unit)

(require racket/match
         "../../rep/measure-unit-rep.rkt"
         )

(define (print-measure-unit type port write?)
  (display (measure-unit->sexp type) port))

(define (measure-unit->sexp u)
  (match-define (measure-unit: ht) u)
  `(u* ,@(for/list ([(k v) ht])
           (match-define (list nm id) k)
           `(u^ ,nm ,v))))
