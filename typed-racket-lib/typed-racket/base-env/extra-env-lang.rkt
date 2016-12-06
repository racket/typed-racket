#lang racket/base

;; This module defines a #lang for use in defining extra base
;; type environments that will only be included on a `require`
;; (unlike the monolithic base type environment in base-env.rkt)
;;
;; Also see env-lang.rkt

(require "../utils/utils.rkt"
         (for-syntax (private parse-type))
         (for-syntax "type-name-error.rkt"
                     racket/base
                     syntax/parse
                     syntax/stx)
         (for-syntax (types abbrev numeric-tower prop-ops)))

(provide type-environment
         (rename-out [-#%module-begin #%module-begin])
         require
         (for-syntax parse-type) ; to allow resolution of Name types
         (except-out (all-from-out racket/base) #%module-begin)
         (for-syntax (except-out (all-from-out racket/base) #%module-begin))
         types rep private utils
         (for-syntax (types-out abbrev numeric-tower prop-ops)))

;; syntax classes for type clauses in the type-environment macro
(begin-for-syntax
  (define-syntax-class clause
    ;; form       - syntax to put in the #%type-decl submodule
    ;; outer-form - other forms to put in outer module
    #:attributes (form outer-form)
    (pattern :simple-clause)
    (pattern :opaque-clause)
    (pattern :struct-clause))

  (define-syntax-class simple-clause
    #:description "[id type]"
    (pattern [id:identifier ty]
             #:with form #'(register-type (quote-syntax id) ty)
             #:with outer-form #'(provide id)))

  (define-syntax-class opaque-clause
    #:description "[#:opaque type pred]"
    (pattern [#:opaque type:id pred:id]
             #:with form
             #'(begin
                 (register-type (quote-syntax id)
                                (make-pred-ty (make-Opaque #'pred)))
                 (register-type-name (quote-syntax type)
                                     (make-Opaque #'pred)))
             #:with outer-form #'(begin
                                   (define-syntax type type-name-error)
                                   (provide type pred))))

  (define-syntax-class struct-clause
    #:description "[#:struct name ([field : type] ...)]"
    ;; FIXME: support other struct options
    (pattern [#:struct name:id ([field:id (~datum :) type:expr] ...)
                       (~optional (~seq #:extra-constructor-name extra:id)
                                  #:defaults ([extra #f]))
                       (~optional (~and (~seq #:no-provide) (~bind [provide? #f]))
                                  #:defaults ([provide? #t]))]
             #:with form #'(d-s name ([field : type] ...))
             #:with outer-form (if (attribute provide?)
                                   #'(provide (struct-out name))
                                   #'(void)))
    (pattern [#:struct (name:id par:id)
                       ([field:id (~datum :) type:expr] ...)
                       (par-type:expr ...)
                       (~optional (~seq #:extra-constructor-name extra:id)
                                  #:defaults ([extra #f]))
                       (~optional (~and (~seq #:no-provide) (~bind [provide? #f]))
                                  #:defaults ([provide? #t]))]
             #:with form #'(d-s (name par) ([field : type] ...) (par-type ...))
             #:with outer-form (if (attribute provide?)
                                   #'(provide (struct-out name))
                                   #'(void)))))

(define-syntax (-#%module-begin stx)
  (syntax-parse stx
    [(mb e ...)
     #'(#%plain-module-begin
        (require (for-syntax typed-racket/env/env-req))
        e ...
        ;; need to register this module
        (begin-for-syntax (add-mod! (variable-reference->module-path-index
                                     (#%variable-reference)))))]))

;; macro that actually sets up the #%type-decl, should be used
;; at most once per extra-env-lang module
(define-syntax (type-environment stx)
  (syntax-parse stx
    [(_ binding:clause ...)
     #'(begin
         (begin-for-syntax
          (module* #%type-decl #f
                   (#%plain-module-begin ;; avoid top-level printing and config
                    (#%declare #:empty-namespace) ;; avoid binding info from here
                    (require typed-racket/types/numeric-tower typed-racket/env/type-name-env
                             typed-racket/env/global-env typed-racket/env/type-alias-env
                             typed-racket/types/struct-table typed-racket/types/abbrev
                             typed-racket/typecheck/tc-structs
                             (only-in typed-racket/rep/type-rep make-Name make-Opaque)
                             (rename-in racket/private/sort [sort raw-sort]))
                    ;; FIXME: add a switch to turn contracts on for testing
                    binding.form ...)))
         binding.outer-form ...)]))

