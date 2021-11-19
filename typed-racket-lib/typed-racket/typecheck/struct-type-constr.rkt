#lang racket/base

(require "../rep/type-constr.rkt"
         (only-in "../types/utils.rkt" instantiate-poly))

(provide (struct-out struct-type-op))
(struct struct-type-op (sty) #:transparent
  #:methods gen:type-rep-maker
  [(define (gen-create-type-rep me args)
     (instantiate-poly (struct-type-op-sty me) args))
   (define (gen-serialize-type-rep me ty->sexp)
     `(struct-type-op ,(ty->sexp (struct-type-op-sty me))))])
