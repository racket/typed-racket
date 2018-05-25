#lang racket/base
(require
  syntax/parse/pre
  (for-syntax racket/base syntax/parse/pre racket/syntax))

(define-syntax define-matcher
  (syntax-parser
    [(_ name:id prop:id)
     #'(define-syntax-class name
         #:attributes (value)
         (pattern e
          #:attr value (prop #'e)
          #:when (attribute value)))]))

(define-syntax (define-properties stx)
  (define-syntax-class clause
    (pattern (name:id sym:id #:mark)
      #:with syntax-class-name (format-id #'name "~a^" #'name)
      #:with symbol (generate-temporary #'sym)
      #:with function
        #'(λ (stx) (syntax-property stx symbol #t)))
    (pattern (root:id sym:id)
      #:with name (format-id #'root "~a-property" #'root)
      #:with syntax-class-name (format-id #'root "~a^" #'root)
      #:with symbol (generate-temporary #'sym)
      #:with function
        #'(case-lambda
            ((stx) (syntax-property stx symbol))
            ((stx value) (syntax-property stx symbol value)))))

  (syntax-parse stx
    [(_ :clause ...)
     #`(begin
         (begin
            ;; TODO: make this an uninterned symbol once the phasing issue of the unit
            ;; tests is fixed
            (define symbol 'sym)
            (provide name syntax-class-name)
            (define name function)
            (define-syntax-class syntax-class-name
              #:attributes (value)
              (pattern e
               #:attr value (syntax-property #'e symbol)
               #:when (attribute value)))) ...)]))

;;TODO add contracts on the properties

(define-properties
  (plambda typechecker:plambda)
  (ignore typechecker:ignore #:mark)
  (ignore-some typechecker:ignore-some #:mark)
  (ignore-some-expr typechecker:ignore-some)
  (contract-def typechecker:contract-def) ; -> Contract-Def (struct in type-contract.rkt)
  (contract-def/provide typechecker:contract-def/provide)
  (external-check typechecker:external-check)
  (casted-expr typechecker:casted-expr) ; Type -> Void, takes the original type of the casted expr
  (with-type typechecker:with-type #:mark)
  (type-ascription type-ascription)
  (type-inst type-inst)
  (row-inst row-inst)
  (type-label type-label)
  (optional-non-immediate-arg optional-non-immediate-arg)
  (optional-immediate-arg optional-immediate-arg)
  (type-dotted type-dotted)
  (exn-predicate typechecker:exn-predicate)
  (exn-handler typechecker:exn-handler)
  (exn-body typechecker:exn-body #:mark)
  (exn-handlers typechecker:exn-handlers #:mark)
  (struct-info struct-info)
  (opt-lambda opt-lambda)
  (kw-lambda kw-lambda)
  (tail-position typechecker:called-in-tail-position #:mark)
  (tr:class tr:class #:mark)
  (tr:class:top-level tr:class:top-level)
  (tr:class:super-new tr:class:super-new)
  (tr:class:type-annotation tr:class:type-annotation)
  (tr:class:super tr:class:super)
  (tr:class:local-table tr:class:local-table)
  (tr:class:name-table tr:class:name-table)
  (tr:class:def tr:class:def)
  (tr:unit tr:unit #:mark)
  (tr:unit:body-exp-def-type tr:unit:body-exp-def-type)
  (tr:unit:invoke tr:unit:invoke)
  (tr:unit:invoke:expr tr:unit:invoke:expr)
  (tr:unit:compound tr:unit:compound)
  (tr:unit:from-context tr:unit:from-context #:mark)
  (unsafe-provide unsafe-provide #:mark)
  (typed-racket:ignore-type-information typed-racket:ignore-type-information))
