#lang racket/base

(require (for-syntax racket/base
                     syntax/parse
                     syntax/stx
                     racket/syntax
                     typed-racket/utils/tc-utils
                     typed-racket/typecheck/renamer)
         typed-racket/utils/tc-utils)

(provide syntax-local-typed-context?
         define-typed/untyped-identifier
         require/untyped-contract)

(define (syntax-local-typed-context?)
  (unbox typed-context?))

(define-syntax (define-typed/untyped-identifier stx)
  (syntax-parse stx
    [(_ name:id typed-name:id untyped-name:id)
     (syntax/loc stx
       (define-syntax name
         (make-typed-renaming #'typed-name #'untyped-name)))]))

(define-for-syntax (freshen ids)
  (stx-map (lambda (id) ((make-syntax-introducer) id)) ids))

(define-syntax (require/untyped-contract stx)
  (syntax-parse stx #:literals (begin)
    [(_ (begin form ...) from-module-spec:expr [name:id T:expr] ...)
     (with-syntax* ([(typed-name ...)  (generate-temporaries #'(name ...))]
                    [(untyped-name ...)  (freshen #'(name ...))]
                    [(untyped2-name ...)  (generate-temporaries #'(name ...))]
                    [(untyped3-name ...)  (generate-temporaries #'(name ...))]
                    [(macro-name ...)  (generate-temporaries #'(name ...))]
                    [typed-module  (generate-temporary #'typed-module)]
                    [untyped-module  (generate-temporary #'untyped-module)]
                    [*racket/base (datum->syntax #'from-module-spec 'racket/base)]
                    [*typed/racket/base (datum->syntax #'from-module-spec
                                                       'typed/racket/base)]
                    [*require (datum->syntax #'from-module-spec
                                             'require)])
       (syntax/loc stx
         (begin
           (module typed-module *typed/racket/base ; to bind in `T`s
             (*require typed/racket/base) ; to bind introduced `begin`, etc.
             (begin form ...)
             (require (only-in from-module-spec
                               [name untyped2-name] ...))
             (provide untyped-name ...)
             (: untyped-name T) ...
             (define untyped-name untyped2-name) ...)
           
           (module untyped-module *racket/base
             (*require racket/base)
             (require typed/untyped-utils
                      (only-in from-module-spec
                               [name typed-name] ...)
                      (only-in (submod ".." typed-module)
                               [untyped-name untyped3-name] ...))
             (provide macro-name ...)
             (define-typed/untyped-identifier macro-name typed-name untyped3-name) ...)
           
           (require (rename-in (submod "." untyped-module) [macro-name name] ...)))))]
    [(_ from-module-spec:expr [name:id T:expr] ...)
     (syntax/loc stx (require/untyped-contract (begin) from-module-spec [name T] ...))]))
