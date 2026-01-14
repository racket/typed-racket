#lang racket/base
(require "../rep/type-constr.rkt"
         racket/match
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


;; Check if a type constructor is for a polymorphic struct type
(define (poly-struct-type-constr? constr)
  (match constr
    [(TypeConstructor (user-defined-type-op _ _ _ poly-struct?) _ _ _ _)
     poly-struct?]
    [_ #f]))

;; returns true if id refers to a built-in or non-recursive type constructor
;; that is NOT a polymorphic struct type constructor
;; (polymorphic struct type constructors should NOT be inlined during parsing
;; so that App types are created, allowing the printer to show type arguments)
(define (simple-type-constructor? id)
  (cond
    [(lookup-type-constructor id)
     =>
     (lambda (constr)
       (and (not (and (user-defined-type-constr? constr)
                      (recursive-type-constr? constr)))
            ;; Don't inline polymorphic struct type constructors
            ;; so that App types are created for proper printing
            (not (poly-struct-type-constr? constr))))]
    [else #f]))
