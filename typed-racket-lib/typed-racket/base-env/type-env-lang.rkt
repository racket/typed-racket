#lang racket/base

(require (for-syntax racket/base syntax/parse/pre syntax/stx))

(begin-for-syntax
  (define-syntax-class t-name
    #:attributes (nm ty export)
    (pattern (nm:id ty)
             #:attr export #'(provide nm))
    (pattern (nm:id ty #:alias (alias ...))
             #:attr export #'(provide nm (rename-out [nm alias] ...)))))

(define-syntax (#%module-begin stx)
  (syntax-parse stx
    #:literals (require)
    [(mb (require . args) ... name:t-name ...)
     #'(#%plain-module-begin
        (begin
          (define-syntax (name.nm stx)
            (raise-syntax-error
             'type-check
             (format "type name used out of context\n  type: ~a\n  in: ~a"
                     (syntax->datum (if (stx-pair? stx) (stx-car stx) stx))
                     (syntax->datum stx))
             stx
             (and (stx-pair? stx) (stx-car stx))))
          ...
          name.export ...
          (begin-for-syntax
            (module* initialize #f
              (require
               (only-in typed-racket/env/init-envs initialize-type-name-env))
              (require . args) ...
              (provide initialize-type-names)
              (define (initialize-type-names)
                (initialize-type-name-env
                 (list (list #'name.nm name.ty) ...)))))))]))

(provide #%module-begin require
         (all-from-out racket/base)
         (for-syntax (all-from-out racket/base)))
