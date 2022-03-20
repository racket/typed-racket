#lang racket/base

;; This file is logically part of "prims.rkt" but is in a separate
;; file to avoid cyclic module dependencies
;;
;; In particular, "parse-type.rkt" needs the binding of the TR
;; case-lambda in order to match for case-lambda types.

(require "colon.rkt"
         "ann-inst.rkt"
         (for-syntax "annotate-classes.rkt"
                     "../private/syntax-properties.rkt"
                     racket/base
                     racket/syntax
                     syntax/stx
                     syntax/parse/pre))

(provide (rename-out [-case-lambda case-lambda]
                     [-case-lambda case-lambda:]
                     [lambda: λ:])
         pcase-lambda:
         plambda:
         -lambda
         lambda:
         popt-lambda:
         opt-lambda:)

(define-syntax (lambda: stx)
  (syntax-parse stx
    [(lambda: formals:annotated-formals . body)
     (syntax/loc stx (-lambda formals.ann-formals . body))]))

(define-syntax (opt-lambda: stx)
  (syntax-parse stx
    [(opt-lambda: formals:opt-lambda-annotated-formals . body)
     (syntax/loc stx (-lambda formals.ann-formals . body))]))

(define-syntax (plambda: stx)
  (syntax-parse stx
    [(plambda: tvars:type-variables formals . body)
     (plambda-property
       (syntax/loc stx (lambda: formals . body))
       #'(tvars.vars ...)) ]))

(define-syntax (popt-lambda: stx)
  (syntax-parse stx
    [(popt-lambda: tvars:type-variables formals . body)
     (plambda-property
       (syntax/loc stx (opt-lambda: formals . body))
       #'(tvars.vars ...))]))


(begin-for-syntax
  (define-syntax-class case-lambda-formals
    (pattern (~or (formal:optionally-annotated-formal ... . rst:rest-arg)
                  (~and (formal:optionally-annotated-formal ...)
                        (~bind [rst.form #'()])))
             #:with form
             (syntax/loc this-syntax
               (formal.ann-name ... . rst.form)))))

(define-syntax (-case-lambda stx)
  (syntax-parse stx
    [(_ vars:maybe-lambda-type-vars
        [formals:case-lambda-formals . body] ...)
     (plambda-property
      (syntax/loc stx
        (case-lambda [formals.form . body] ...))
      (attribute vars.type-vars))]))

(define-syntax (pcase-lambda: stx)
  (syntax-parse stx
    [(pcase-lambda: tvars:type-variables cl ...)
     (plambda-property
       (syntax/loc stx (-case-lambda cl ...))
       #'(tvars.vars ...))]))

;; lambda with optional type annotations, uses syntax properties
(define-syntax (-lambda stx)
  (syntax-parse stx
    #:literals (:)
    [(_ vars:maybe-lambda-type-vars
        formals:lambda-formals
        return:return-ann
        (~describe "body expression or definition" e) ...
        (~describe "body expression" last-e))
     ;; Annotate the last expression with the return type. Should be correct
     ;; since if a function returns, it has to do so through the last expression
     ;; even with continuations.
     (define/with-syntax last-e*
       (if (attribute return.type)
           #`(ann last-e #,(attribute return.type))
           #'last-e))

     ;; if the lambda form being checked is a polymorphic function, tag its
     ;; parameters with property `from-lambda'.
     (define (maybe-set-from-lambda type-vars formals)
       (cond
         [(and type-vars (stx-list? formals))
          (stx-map (lambda (x)
                     (from-plambda-property x #t))
                   formals)]
         [type-vars (from-plambda-property formals #t)]
         [else formals]))

     (define d (with-syntax ([erase-formals (maybe-set-from-lambda
                                             (attribute vars.type-vars)
                                             (attribute formals.erased))])
                 (syntax/loc stx (λ erase-formals e ... last-e*))))
     (define d/prop
       (if (attribute formals.kw-property)
           (kw-lambda-property d (attribute formals.kw-property))
           (opt-lambda-property d (attribute formals.opt-property))))
     ;; attach a plambda property if necessary
     (if (attribute vars.type-vars)
         (plambda-property d/prop (attribute vars.type-vars))
         d/prop)]))
