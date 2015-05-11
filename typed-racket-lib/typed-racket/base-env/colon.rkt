#lang racket/base

(require (for-syntax racket/base syntax/parse/pre unstable/syntax
                     "../private/parse-classes.rkt"
                     "../utils/disappeared-use.rkt"
                     (only-in "../utils/tc-utils.rkt" tc-error/stx))
         (submod "../typecheck/internal-forms.rkt" forms)
         (prefix-in t: "base-types-extra.rkt"))

(provide :)

(begin-for-syntax
 (define (err stx str . sub)
   (apply raise-syntax-error '|type declaration| str stx sub))

 ;; Wrap the `:-expr` with a `define-values`. This is like
 ;; what `internal` does, but the work is spread out among two
 ;; macros to delay the unbound identifier check.
 (define (wrap stx :-expr)
   (quasisyntax/loc stx (define-values () #,:-expr))))

(define-syntax (: stx)
  (define ctx (syntax-local-context))
  (define top-level? (eq? 'top-level ctx))
  ;; make it possible to add another colon after the id for clarity
  ;; and in that case, a `->' on the RHS does not need to be
  ;; explicitly parenthesized
  (syntax-parse stx #:literals (: t:->)
    [_
     #:when (eq? 'expression ctx)
     (err stx "must be used in a definition context")]
    [(: id (~and kw :) . more:omit-parens)
     (add-disappeared-use #'kw)
     (wrap stx #`(:-helper #,stx #,top-level? id more.type))]
    [(: e ...)
     (wrap stx #`(:-helper #,stx #,top-level? e ...))]))

(define-syntax (:-helper stx)
  (syntax-parse stx
    [(_ orig-stx top-level? i ty)
     #:fail-unless (identifier? #'i) (err #'orig-stx "expected identifier" #'i)
     (unless (or (syntax-e #'top-level?)
                 (identifier-binding #'i))
       (tc-error/stx #'i
                     "Declaration for `~a' provided, but `~a' has no definition"
                     (syntax-e #'i)
                     (syntax-e #'i)))
     (syntax-property (syntax/loc stx (begin (quote (:-internal i ty))
                                             (#%plain-app values)))
                      'disappeared-use #'i)]
    [(_ orig-stx _ i x ...)
     #:fail-unless (identifier? #'i) (err #'orig-stx "expected identifier" #'i)
     (case (syntax-length #'(x ...))
       [(0)  (err #'orig-stx "missing type after identifier")]
       [else (err #'orig-stx "too many types after identifier")])]))

