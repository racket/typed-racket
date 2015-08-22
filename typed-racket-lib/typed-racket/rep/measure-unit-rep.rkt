#lang racket/base

(provide make-measure-unit make-base-measure-unit measure-unit:
         make-measure-unit/F make-F-measure-unit measure-unit/F: make-measure-unit/maybe-F
         apply/F-measure-units app/F-measure-units)

(require racket/match
         racket/contract/base
         racket/list
         "rep-utils.rkt"
         "free-variance.rkt"
         (for-syntax racket/base
                     syntax/parse
                     ))

;; a base-measure-unit/c is not a measure-unit
(define base-measure-unit/c (list/c symbol? symbol?))

(def-measure-unit Measure-Unit
  ([hash-table
    (hash/c base-measure-unit/c (and/c exact-integer? (not/c zero?)) #:immutable #t)])
  [#:frees #f])

;; analogous to F in type-rep.rkt
(def-measure-unit Measure-Unit/F
  ([deps (listof symbol?)] [u Measure-Unit?])
  ;; deps is a list of symbols which will match the second symbol of a
  ;;   base-measure-unit/c in u
  [#:frees (λ (f) (combine-frees (list* (f u) (map single-free-var deps))))])

(define (make-measure-unit ht)
  (*Measure-Unit
   (for/hash ([(k v) (in-hash ht)] #:unless (zero? v))
     (values k v))))

(define (make-measure-unit/F deps u)
  (*Measure-Unit/F deps u))

(define (make-measure-unit/maybe-F deps u)
  (cond [(empty? deps) u]
        [else (make-measure-unit/F deps u)]))

(define (make-base-measure-unit name id)
  (make-measure-unit (hash (list name id) 1)))

(define (make-F-measure-unit id)
  (make-measure-unit/F (list id) (make-base-measure-unit id id)))

(define-match-expander measure-unit:
  (syntax-parser
    [(measure-unit: pat)
     #'(Measure-Unit: pat)]))

(define-match-expander measure-unit/F:
  (syntax-parser
    [(measure-unit/F: deps u)
     #'(Measure-Unit/F: deps u)]))

(define (apply/F-measure-units f args)
  (define-values [depss args*]
    (for/lists [depss args*] ([arg (in-list args)])
      (match arg
        [(measure-unit/F: deps a)
         (values deps a)]
        [a
         (values '() a)])))
  (define deps (remove-duplicates (append* depss)))
  (make-measure-unit/maybe-F deps (apply f args*)))

(define (app/F-measure-units f . args)
  (apply/F-measure-units f args))
