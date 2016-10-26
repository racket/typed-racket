#lang racket/base

;; Manages a set of mutually recursive equations, and provids functionality for finding a fix point.
;; An equation set has two components
;;  1. a mapping of variables to initial values.
;;  2. a mapping of variables to thunks that compute new values.


(provide
  make-equation-set
  add-variable!
  add-equation!
  resolve-equations
  variable-ref)

; equations:(hash/c symbol? value?)
; initial-values: (hash/c symbol?  value?)
(struct equation-set (equations initial-values))

(define (make-equation-set)
  (equation-set (make-hasheq) (make-hasheq)))

; add-variable!: (equation-set? value? -> var?)
(define (add-variable! eqs initial-value)
  (define a-var (gensym 'var))
  (hash-set! (equation-set-initial-values eqs) a-var initial-value)
  a-var)

; add-equation!: (equation-set? var? (-> value?) -> void?)
(define (add-equation! eqs var thunk)
  (hash-set! (equation-set-equations eqs) var thunk))

(define current-variable-values (make-parameter (make-hasheq)))

;; resolve-equations (equation-set? -> (hash/c symbol? value?)
;; Produces a mapping of variables to values such that every equation holds.
(define (resolve-equations eqs)
  (define vals (make-hasheq))
  (for ([(key val) (in-hash (equation-set-initial-values eqs))])
    (hash-set! vals key val))
  (parameterize ((current-variable-values vals))
    (let loop ()
      (define change #f) 
      (for ([(v thunk) (in-hash (equation-set-equations eqs))])
        (define new-value (thunk))
        (define old-value (hash-ref vals v))
        (unless (equal? new-value old-value)
          (set! change #t)
          (hash-set! vals v new-value)))
      (when change
        (loop)))
    vals))

(define (variable-ref v)
  (hash-ref (current-variable-values) v (λ () (error 'variable-ref "No value available."))))
