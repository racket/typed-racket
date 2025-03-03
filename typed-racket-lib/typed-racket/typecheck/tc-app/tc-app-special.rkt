#lang racket/unit

(require "../../utils/utils.rkt"
         "signatures.rkt"
         "utils.rkt"
         syntax/parse racket/match
         syntax/stx
         racket/sequence
         "../signatures.rkt"
         "../tc-funapp.rkt"
         "../../types/abbrev.rkt"
         "../../types/type-table.rkt"
         "../../types/utils.rkt"
         "../../private/type-annotation.rkt"
         "../../rep/type-rep.rkt"
         "../../rep/prop-rep.rkt"
         "../../utils/tc-utils.rkt"
         (for-label racket/base racket/bool '#%paramz))


(import tc-expr^)
(export tc-app-special^)

(define-literal-set special-literals #:for-label
  (extend-parameterization false? not call-with-values list))

(define-tc/app-syntax-class (tc/app-special expected)
  #:literal-sets (kernel-literals special-literals)
  ;; parameterize
  (pattern (extend-parameterization pmz (~seq params args) ...)
    (begin
      (register-ignored! #'pmz)
      (for ([param (in-syntax #'(params ...))]
            [arg (in-syntax #'(args ...))])
        (match (single-value param)
          [(tc-result1: (Param: a b))
           (tc-expr/check arg (ret a))]
          [(tc-result1: t)
           (single-value arg)
           (tc-error/delayed "expected Parameter, but got ~a" t)]))
      (ret Univ)))
  ;; use the additional but normally ignored first argument to make-sequence
  ;; to provide a better instantiation
  (pattern ((~var op (id-from 'make-sequence 'racket/private/for))
            (~and quo (quote (i:id ...))) arg:expr)
    #:do [(define i-anns
            (for/list ([id (in-list (syntax-e #'(i ...)))])
              (type-annotation id #:lookup? #false)))]
    #:when (andmap values i-anns)
    (match (single-value #'op)
        [(tc-result1: (and t Poly?))
         (tc-expr/check #'quo (ret Univ))
         (tc/funapp #'op #'(quo arg)
                    (instantiate-poly t i-anns)
                    (list (ret Univ) (single-value #'arg))
                    expected)]))
  ;; special-case for not - flip the props
  (pattern ((~and op-name (~or false? not)) arg)
    (match (single-value #'arg)
      [(tc-result1: t (PropSet: p+ p-) _)
       (define new-prop (make-PropSet p- p+))
       (add-typeof-expr #'op-name (ret (-> Univ -Boolean :T+ #t)))
       (ret -Boolean new-prop)]))
  ;; special case for (current-contract-region)'s default expansion
  ;; just let it through without any typechecking, since module-name-fixup
  ;; is a private function from syntax/location, so this must have been
  ;; (quote-module-name) originally.
  (pattern (op src path)
    #:declare op (id-from 'module-name-fixup 'syntax/location)
    (ret Univ))
  ;; special case for `delay'
  (pattern (mp1 (#%plain-lambda ()
                  (#%plain-app mp2 (#%plain-app call-with-values (#%plain-lambda () e) list))))
    #:declare mp1 (id-from 'make-promise 'racket/promise)
    #:declare mp2 (id-from 'make-promise 'racket/promise)
    (ret (-Promise (tc-expr/t #'e)))))
