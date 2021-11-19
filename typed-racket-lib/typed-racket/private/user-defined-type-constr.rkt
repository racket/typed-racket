#lang racket/base
(require  "../rep/type-constr.rkt"
          racket/match
          racket/lazy-require
          (only-in "../types/substitute.rkt" subst-all make-simple-substitution))

(provide (struct-out user-defined-type-op))

(struct user-defined-type-op (vars type) #:transparent
  #:methods gen:type-rep-maker
  [(define (gen-create-type-rep me args)
     (match-define (user-defined-type-op vars type) me)
     (subst-all (make-simple-substitution vars args)
                type))
   (define (gen-serialize-type-rep me t->s)
     (match-define (user-defined-type-op vars type) me)
     `(user-defined-type-op (list ,@(for/list ([i (in-list vars)])
                                      `(quote ,i)))
                            ,(t->s type)))])
