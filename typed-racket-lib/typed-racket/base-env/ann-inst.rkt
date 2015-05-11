#lang racket/base

;; Forms for adding type annotations.

;; This file is loaded by all Typed Racket programs, so it should not
;; have expensive runtime dependencies.

(require (for-syntax syntax/parse/pre "../private/syntax-properties.rkt"
                     racket/base)
         "colon.rkt")

(provide (for-syntax add-ann) ann inst)

(define-syntax (ann stx)
  (syntax-parse stx #:literals (:)
    [(_ (~or (~seq arg : ty) (~seq arg ty)))
     (add-ann #'arg #'ty)]))

(define-for-syntax (add-ann expr-stx ty-stx)
  (quasisyntax/loc expr-stx
    (#,(type-ascription-property #'#%expression ty-stx)
     #,expr-stx)))

(define-syntax (inst stx)
  (syntax-parse stx #:literals (:)
    [(_ arg : . tys)
     (syntax/loc stx (inst arg . tys))]
    ;; FIXME: Is the right choice to use a #:row keyword or just
    ;; to use a Row type constructor and keep it consistent?
    [(_ arg #:row e ...)
     (with-syntax ([expr (type-inst-property #'#%expression #'(#:row e ...))])
       (syntax/loc #'arg (expr arg)))]
    [(_ arg tys ... ty ddd b:id)
     #:when (eq? (syntax-e #'ddd) '...)
     (with-syntax ([expr (type-inst-property #'#%expression #'(tys ... (ty . b)))])
       (syntax/loc #'arg (expr arg)))]
    [(_ arg tys ...)
     (with-syntax ([expr (type-inst-property #'#%expression #'(tys ...))])
       (syntax/loc #'arg (expr arg)))]))
