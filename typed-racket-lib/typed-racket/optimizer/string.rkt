#lang racket/base

(require (for-template racket/base
                       racket/unsafe/ops)
         syntax/parse
         "../utils/utils.rkt"
         "logging.rkt"
         "utils.rkt")

(provide string-opt-expr)

(define-unsafe-syntax-class string-length)
(define-unsafe-syntax-class bytes-length)


(define-syntax-class string-opt-expr
  #:commit
  #:literal-sets (kernel-literals)
  (pattern (#%plain-app op:string-length^ s:opt-expr)
    #:do [(log-opt "string-length" "String check elimination.")]
    #:with opt #'(op.unsafe s.opt))
  (pattern (#%plain-app op:bytes-length^ s:opt-expr)
    #:do [(log-opt "bytes-length" "Byte string check elimination.")]
    #:with opt #'(op.unsafe s.opt)))
