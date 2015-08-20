#lang racket/base

(provide make-measure-unit make-base-measure-unit measure-unit:)

(require racket/match
         racket/contract/base
         "rep-utils.rkt"
         (for-syntax racket/base
                     syntax/parse
                     ))

;; a base-measure-unit/c is not a measure-unit
(define base-measure-unit/c (list/c symbol? symbol?))

(def-measure-unit Measure-Unit
  ([hash-table
    (hash/c base-measure-unit/c (and/c exact-integer? (not/c zero?)))])
  [#:frees #f])

(define (make-measure-unit ht)
  (*Measure-Unit
   (for/hash ([(k v) (in-hash ht)] #:unless (zero? v))
     (values k v))))

(define (make-base-measure-unit name id)
  (make-measure-unit (hash (list name id) 1)))

(define-match-expander measure-unit:
  (syntax-parser
    [(measure-unit: pat)
     #'(Measure-Unit: pat)]))
