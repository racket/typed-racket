#lang racket/base
(require "../rep/type-constr.rkt"
         syntax/id-table
         "../env/env-utils.rkt"
         "../private/user-defined-type-constr.rkt"
         "../typecheck/renamer.rkt")

(provide register-type-constructor!
         lookup-type-constructor
         simple-type-constructor?
         kind-env-map)

(define kind-env (make-free-id-table))

(define (kind-env-map f)
  (for/list ([(id constr) (in-sorted-free-id-table kind-env)])
    (f id constr)))


(define (lookup-type-constructor id)
  (or (free-id-table-ref kind-env id #f)
      (free-id-table-ref kind-env (un-rename id) #f)))

(define (register-type-constructor! name type-constr)
  (free-id-table-set! kind-env name type-constr))


;; returns true if id refers to a built-in or non-recursive type constructor
(define (simple-type-constructor? id)
  (cond
    [(lookup-type-constructor id)
     =>
     (lambda (constr)
       (not (and (user-defined-type-constr? constr)
                 (recursive-type-constr? constr))))]
    [else #f]))
