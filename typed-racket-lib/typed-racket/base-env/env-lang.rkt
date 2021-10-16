#lang racket/base

(require "../utils/utils.rkt")

(require (for-syntax racket/base syntax/parse)
         "../utils/tc-utils.rkt"
         "../env/init-envs.rkt"
         "../types/abbrev.rkt"
         "../types/numeric-tower.rkt"
         "../types/prop-ops.rkt")

(define-syntax (-#%module-begin stx)
  (define-syntax-class clause
    #:description "[id type]"
    (pattern [id:identifier ty]))
  (syntax-parse stx #:literals (require begin)
    [(mb (~optional (~and extra (~or (begin . _) (require . args))))
         ~! :clause ...)
     #'(#%plain-module-begin
        (begin
          extra
          (define org-map
            (make-env [id (λ () ty)] ...))
          (define (init)
           (initialize-type-env org-map))
          (provide init org-map)))]
    [(mb . rest)
     #'(mb (begin) . rest)]))

(provide (rename-out [-#%module-begin #%module-begin])
         require
         (except-out (all-from-out racket/base) #%module-begin)
         (combine-out (all-from-out "../types/abbrev.rkt" "../types/numeric-tower.rkt" "../types/prop-ops.rkt")))
