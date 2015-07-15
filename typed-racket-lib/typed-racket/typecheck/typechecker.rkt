#lang racket/base

(require (only-in racket/unit
                  provide-signature-elements
                  define-values/invoke-unit/infer link)
         "signatures.rkt"
         "tc-app-combined.rkt"
         "tc-if.rkt" "tc-lambda-unit.rkt"
         "tc-let-unit.rkt" "tc-apply.rkt"
         "tc-literal.rkt" "tc-expression.rkt"
         "tc-send.rkt"
         "tc-expr-unit.rkt" "check-subforms-unit.rkt"
         "check-class-unit.rkt"
         "check-unit-unit.rkt")

(provide-signature-elements tc-expr^ check-subforms^ tc-literal^)

(define-values/invoke-unit/infer
  (link tc-if@ tc-lambda@ tc-app-combined@ tc-let@ tc-expr@
        tc-send@ check-subforms@ tc-apply@ tc-literal@
        check-class@ check-unit@ tc-expression@))
