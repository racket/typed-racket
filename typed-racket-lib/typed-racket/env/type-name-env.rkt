#lang racket/base

;; Environment for type names

(require "../utils/utils.rkt"
         "env-utils.rkt")

(require syntax/private/id-table
         (contract-req)
         "type-alias-env.rkt"
         "../utils/tc-utils.rkt"
         "../rep/type-rep.rkt"
         "type-constr-env.rkt"
         "../rep/free-variance.rkt"
         "../types/utils.rkt")

(provide/cond-contract [register-type-name
                        (->* (identifier?) (Type?) any)]
                       [register-type-names
                        (-> (listof identifier?) (listof Type?) any)]
                       [add-alias (-> identifier? identifier? any)]
                       [type-name-env-map
                        (-> (-> identifier? (or/c #t Type?) any) any)]
                       [type-name-env-for-each
                        (-> (-> identifier? (or/c #t Type?) any) void?)]
                       [lookup-type-name
                        (->* (identifier?) (procedure?) (or/c #t Type?))])

;; a mapping from id -> type (where id is the name of the type)
(define the-mapping
  (make-free-id-table))

;; add a name to the mapping
(define (register-type-name id [type #t])
  (free-id-table-set! the-mapping id type))

;; add a bunch of names to the mapping
(define (register-type-names ids types)
  (for-each register-type-name ids types))

;; given an identifier, return the type associated with it
;; optional argument is failure continuation - default calls lookup-fail
;; identifier (-> error) -> type
(define (lookup-type-name id [k (lambda () (lookup-type-fail id))])
  (begin0
    (free-id-table-ref the-mapping id k)
    (add-disappeared-use id)))


;; map over the-mapping, producing a list
;; (id type -> T) -> listof[T]
(define (type-name-env-map f)
  (sorted-free-id-table-map the-mapping f))

(define (type-name-env-for-each f)
  (sorted-free-id-table-for-each the-mapping f))

(define (add-alias from to)
  (when (lookup-type-name to (lambda () #f))
    (register-resolved-type-alias
     from
     (make-Name to 0 #t)))
  (cond
    [(lookup-type-constructor to)
     =>
     (lambda (v)
       (register-type-constructor! from v))]))


