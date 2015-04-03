#lang racket/base

(require
 (for-syntax racket/base racket/lazy-require
             "standard-inits.rkt")
 ;; these need to be available to the generated code
 "typecheck/renamer.rkt" syntax/location
 (for-syntax (submod "base-env/prims-contract.rkt" self-ctor))
 (for-syntax "utils/struct-extraction.rkt")
 (for-syntax "typecheck/renamer.rkt")
 ;; only for timing/debugging
 (for-syntax "utils/timing.rkt"))

(provide (rename-out [module-begin #%module-begin]
                     [top-interaction #%top-interaction])
         with-type
         (for-syntax do-standard-inits))


(define-syntax-rule (drivers [name sym] ...)
  (begin
    (begin-for-syntax
      (lazy-require (typed-racket/core (sym ...))))
    (define-syntax (name stx)
      (do-time (format "Calling ~a driver" 'name))      
      (do-time (format "Loaded core ~a" 'sym))
      (begin0 (sym stx)
              (do-time "Finished, returning to Racket")))
    ...))

(drivers [module-begin mb-core] [top-interaction ti-core] [with-type wt-core])
