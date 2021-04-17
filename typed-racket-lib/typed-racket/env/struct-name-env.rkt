#lang racket/base
(require syntax/id-table
         "../typecheck/renamer.rkt"
         "env-utils.rkt")

(provide register-struct-name!
         struct-name-map
         lookup-struct-name)

(define mapping (make-free-id-table))

(define (register-struct-name! sname tname)
  (free-id-table-set! mapping sname tname))

(define (lookup-struct-name sname)
  (or (free-id-table-ref mapping sname #f)
      (free-id-table-ref mapping (un-rename sname) #f)))


(define (struct-name-map f)
  (sorted-free-id-table-map mapping f))
