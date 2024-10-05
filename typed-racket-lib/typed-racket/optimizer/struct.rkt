#lang racket/base

(require (for-template racket/base
                       racket/unsafe/ops)
         syntax/parse
         "../types/struct-table.rkt"
         "../utils/tc-utils.rkt"
         "../utils/utils.rkt"
         "logging.rkt"
         "utils.rkt")

(provide struct-opt-expr)

(define struct-opt-msg "Struct access specialization.")

(define-syntax-class struct-op
  #:attributes (message opt idx)
  (pattern op:id
    #:when (struct-accessor? #'op)
    #:attr message "struct ref"
    #:with idx #`'#,(struct-accessor? #'op)
    #:with opt #'unsafe-struct-ref)
  (pattern op:id
    #:when (struct-mutator? #'op)
    #:attr message "struct set"
    #:with idx #`'#,(struct-mutator? #'op)
    #:with opt #'unsafe-struct-set!))

(define-syntax-class struct-opt-expr
  #:commit
  #:literal-sets (kernel-literals)
  ;; we can always optimize struct accessors and mutators
  ;; if they typecheck, they're safe
  (pattern (#%plain-app op:struct-op s:opt-expr v:opt-expr ...)
    #:do [(add-disappeared-use #'op)
          (log-opt (attribute op.message) struct-opt-msg)]
    #:with opt #'(op.opt s.opt op.idx v.opt ...)))
