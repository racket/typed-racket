#lang racket/base

(provide define-base-measure-unit
         define-measure-unit
         base-measure-unit
         measure
         u*
         u^
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
                     "../../rep/measure-unit-rep.rkt"
                     "parse-measure-unit.rkt"
                     "../../env/measure-unit-env.rkt"
                     "../../private/syntax-properties.rkt"
                     "../../typecheck/internal-forms.rkt"
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
  (define-values (-u* -u^)
    (let ()
      (define u*
        (case-lambda
          [() (make-measure-unit (hash))]
          [(u1 . rst)
           (match-define (measure-unit: ht1) u1)
           (match-define (list (measure-unit: rst-hts) ...) rst)
           (make-measure-unit (apply hash-union ht1 rst-hts #:combine +))]))
      (define (u^ u1 n)
        (match-define (measure-unit: ht) u1)
        (make-measure-unit
         (for/hash ([(k v) (in-hash ht)])
           (define v*n (* v n))
           (unless (integer? v*n)
             (error 'u^ "non-integer result exponent: ~v" v*n))
           (values k v*n))))
      (values u* u^)))
  (register-resolved-measure-unit #'u* -u*)
  (register-resolved-measure-unit #'u^ -u^)

  (define (add-measure-prop expr-stx u-stx)
    (quasisyntax/loc expr-stx
      (#,(measure-property #'#%expression u-stx)
       #,expr-stx)))
  )

(define-syntax measure
  (lambda (stx)
    (syntax-parse stx
      [(measure n:expr u:expr)
       (add-measure-prop #'n #'u)])))

