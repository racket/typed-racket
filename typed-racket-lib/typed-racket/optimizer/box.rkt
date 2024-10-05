#lang racket/base

(require (for-template racket/base
                       racket/unsafe/ops)
         syntax/parse
         "../utils/utils.rkt"
         "logging.rkt"
         "utils.rkt")

(provide box-opt-expr)

;; set-box! is not optimized since the box might be immutable
(define-unsafe-syntax-class unbox)

(define-merged-syntax-class box-op (unbox^))

(define-syntax-class box-opt-expr
  #:commit
  #:literal-sets (kernel-literals)
  (pattern (#%plain-app op:box-op b:opt-expr new:opt-expr ...)
    #:do [(log-opt "box" "Box check elimination.")]
    #:with opt #`(op.unsafe b.opt new.opt ...)))
