#lang racket/unit

(require "../../utils/utils.rkt"
         "signatures.rkt"
         "utils.rkt"
         syntax/parse racket/match
         syntax/stx
         racket/sequence
         (typecheck signatures tc-funapp)
         (types abbrev type-table utils)
         (private type-annotation)
         (rep type-rep prop-rep)
         (utils tc-utils)
         (base-env base-special-env)

         (for-label racket/base racket/bool '#%paramz))


(import tc-expr^ check-contract^)
(export tc-app-special^)

(define-literal-set special-literals #:for-label
  (extend-parameterization false? not call-with-values list))
(define coerce-ctc
  (make-template-identifier 'coerce-contract 'racket/contract/private/guts))

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
    #:when (andmap type-annotation (syntax->list #'(i ...)))
    (match (single-value #'op)
        [(tc-result1: (and t Poly?))
         (tc-expr/check #'quo (ret Univ))
         (tc/funapp #'op #'(quo arg)
                    (instantiate-poly t (list-extend (list Univ Univ)
                                                     (stx-map type-annotation #'(i ...))
                                                     Univ))
                    (list (ret Univ) (single-value #'arg))
                    expected)]))
  ;; special-case for not - flip the props
  (pattern ((~and op-name (~or false? not)) arg)
    (match (single-value #'arg)
      [(tc-result1: t (PropSet: p+ p-) _)
       (define new-prop (make-PropSet p- p+))
       (add-typeof-expr #'op-name (ret (-> Univ -Boolean)))
       (ret -Boolean new-prop)]))
  ;; special case for (current-contract-region)'s default expansion
  ;; just let it through without any typechecking, since module-name-fixup
  ;; is a private function from syntax/location, so this must have been
  ;; (quote-module-name) originally.
  (pattern (op src path)
    #:declare op (id-from 'module-name-fixup 'syntax/location)
    (begin
      ;; Because we aren't typechecking src and path, they won't have entries in
      ;; the type-table. Without ignoring these two, typechecking the expansion
      ;; of provide/contract and contract-out ends up with the optimizer trying
      ;; to type-of src and path... but they have no entry in the type-table!
      (register-ignored! #'src)
      (register-ignored! #'path)
      (ret Univ)))
  ;; special case for `delay'
  (pattern (mp1 (#%plain-lambda ()
                  (#%plain-app mp2 (#%plain-app call-with-values (#%plain-lambda () e) list))))
    #:declare mp1 (id-from 'make-promise 'racket/promise)
    #:declare mp2 (id-from 'make-promise 'racket/promise)
    (ret (-Promise (tc-expr/t #'e))))
  (pattern (app-ctc ctc val pos neg name loc)
    #:declare app-ctc (id-from 'apply-contract 'racket/contract/private/base)
    #:do [(register-ignored! #'loc)]
    (check-contract-app #'ctc #'val))
  ;; coerce-contract is actually a function but its type is somewhat complex, so
  ;; we typecheck it here. If it seems worth it to support this in higher-order
  ;; cases, we can give the identifier a type in contract-prims.
  (pattern (c-c sym e)
    #:when (and (identifier? #'c-c) (free-identifier=? coerce-ctc #'c-c))
    (ret (coerce-to-con (tc-expr/t #'e)))))
