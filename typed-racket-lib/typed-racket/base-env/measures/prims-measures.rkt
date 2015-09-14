#lang racket/base

(provide define-base-measure-unit
         define-measure-unit
         measure
         u*
         u^
         m*
         m^
         m/
         m+
         m-
         )

(require (submod "../../typecheck/internal-forms.rkt" forms)
         "../base-types-extra.rkt"
         "base-measure-unit.rkt"
         ;"ann-inst.rkt"
         (for-syntax racket/base
                     syntax/parse/pre
                     racket/lazy-require
                     syntax/stx
                     racket/list
                     racket/match
                     racket/hash
                     racket/syntax
                     racket/struct-info
                     "parse-measure-unit.rkt"
                     "../../env/measure-unit-env.rkt"
                     "../../private/syntax-properties.rkt"
                     "../../typecheck/internal-forms.rkt"
                     (prefix-in - "../../rep/measure-unit-rep.rkt")
                     ))

(define-syntax define-base-measure-unit
  (syntax-parser
    [(define-base-measure-unit u:id)
     #:with gen (generate-temporary #'u)
     #'(define-measure-unit u (base-measure-unit u gen))]))

(define-syntax define-measure-unit
  (lambda (stx)
    (syntax-parse stx
      [(define-measure-unit name measure-unit)
       #`(begin
           #,(ignore #'(def-measure-unit-stx-err name))
           #,(let ()
               (internal (syntax/loc stx
                           (define-measure-unit-internal name measure-unit))))
           )])))

(def-measure-unit-stx-err u* u^)

(begin-for-syntax
  (register-resolved-measure-unit #'u* -u*)
  (register-resolved-measure-unit #'u^ -u^)
  (register-resolved-measure-unit #'1-measure-unit -1-measure-unit)

  (define (add-measure-prop stx expr-stx u-stx)
    (quasisyntax/loc stx
      (#,(measure-property #'#%expression u-stx)
       #,expr-stx)))
  (define (add-measure-arith-prop stx expr-stx)
    (quasisyntax/loc stx
      (#,(measure-arith-property #'#%expression #t)
       #,expr-stx)))
  )

(define-syntax measure
  (lambda (stx)
    (syntax-parse stx
      [(measure n:expr u:expr)
       (add-measure-prop stx #'n #'u)])))

(define-syntax m*
  (lambda (stx)
    (syntax-parse stx
      [(m* a:expr ...)
       (add-measure-arith-prop stx #'(* a ...))])))

(define-syntax m^
  (lambda (stx)
    (syntax-parse stx
      [(m^ a:expr b:integer)
       (add-measure-arith-prop stx #'(expt a b))])))

(define-syntax m/
  (lambda (stx)
    (syntax-parse stx
      [(m/ a:expr)
       (syntax/loc stx (m^ a -1))]
      [(m/ a:expr b:expr ...+)
       (syntax/loc stx (m* a (m/ (m* b ...))))])))

(define-syntax m+
  (lambda (stx)
    (syntax-parse stx
      [(m+)
       (syntax/loc stx (measure 0 (u*)))]
      [(m+ a:expr ...+)
       (add-measure-arith-prop stx #'(+ a ...))])))

(define-syntax m-
  (lambda (stx)
    (syntax-parse stx
      [(m- a:expr)
       (syntax/loc stx (m* a (measure -1 (u*))))]
      [(m- a:expr b:expr ...+)
       (syntax/loc stx (m+ a (m- (m+ b ...))))])))

