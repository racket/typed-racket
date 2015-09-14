#lang racket/base

(require "../utils/utils.rkt"
         syntax/id-table racket/dict
         (utils tc-utils)
         (typecheck renamer)
         racket/match)

(provide register-measure-unit
         lookup-measure-unit
         resolve-measure-units
         register-resolved-measure-unit
         measure-unit-env-map)

(define-struct measure-unit-def () #:inspector #f)
(define-struct (unresolved measure-unit-def) (stx [in-process #:mutable]) #:inspector #f)
(define-struct (resolved measure-unit-def) (measure-unit) #:inspector #f)

;; a mapping from id -> measure-unit-def (where id is the name of the measure-unit)
(define the-mapping
  (make-free-id-table))

(define (mapping-put! id v) (free-id-table-set! the-mapping id v))

;; add a name to the mapping
;; identifier measure-unit-stx -> void
(define (register-measure-unit id stx)
  (mapping-put! id (make-unresolved stx #f)))

(define (register-resolved-measure-unit id u)
  (mapping-put! id (make-resolved u)))

(define (lookup-measure-unit id parse-measure-unit [k (lambda () (tc-error "Unknown measure-unit: ~a" (syntax-e id)))])
  (match (or (free-id-table-ref the-mapping id #f)
             (free-id-table-ref the-mapping (un-rename id) #f))
    [#f (k)]
    [(struct unresolved (stx #f))
     (resolve-measure-unit id parse-measure-unit)]
    [(struct unresolved (stx #t))
     (tc-error/stx stx "Recursive measure-unit Reference")]
    [(struct resolved (t)) t]))

(define (resolve-measure-unit id parse-measure-unit)
  (define v (free-id-table-ref the-mapping id))
  (match v
    [(struct unresolved (stx _))
     (set-unresolved-in-process! v #t)
     (let ([t (parse-measure-unit stx)])
       (mapping-put! id (make-resolved t))
       t)]
    [(struct resolved (t))
     t]))

(define (resolve-measure-units parse-measure-unit)
  (for ([id (in-dict-keys the-mapping)])
    (resolve-measure-unit id parse-measure-unit)))

;; map over the-mapping, producing a list
;; (id u -> T) -> listof[T]
(define (measure-unit-env-map f)  
  (for/list ([(id u) (in-dict the-mapping)]
             #:when (resolved? u))
    (f id (resolved-measure-unit u))))
