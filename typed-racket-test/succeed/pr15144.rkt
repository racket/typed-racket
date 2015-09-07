#lang typed/racket

;; Caching in the type->sc translation was causing open sc terms to appear in
;; the contract for `bar`.

(define-type (Base-Shape R)
  (U (foo-shape R)
     (bar-shape R)))

(define-syntax (def-base-shape stx)
  (syntax-case stx ()
    [(_ (R (func-name shape-name) [param-name : param-type] ...))
     (syntax/loc stx
       (begin
         (struct (R) shape-name ([param-name : param-type] ...))
         (define #:forall (R) (func-name [param-name : param-type] ...)
           (shape-name param-name ...))))]))

(def-base-shape (R (foo-func foo-shape) [s : (Base-Shape R)]))
(def-base-shape (R (bar-func bar-shape) [s : R]))

(provide foo)
(define (foo [shapes : (Base-Shape String)])
  (foo-func shapes))

(provide bar)
(define (bar [shapes : (Base-Shape String)])
  (bar-func shapes))
