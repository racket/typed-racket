#lang racket/base
(require "../rep/type-constr.rkt"
         racket/match
         racket/lazy-require)

(lazy-require ["../types/substitute.rkt"
               (subst-all make-simple-substitution)])

(provide (struct-out user-defined-type-op)
         user-defined-type-constr?
         recursive-type-constr?)

(struct user-defined-type-op (vars type recursive?) #:transparent
  #:methods gen:type-rep-maker
  [(define (gen-create-type-rep me args)
     (match-define (user-defined-type-op vars type recursive?) me)
     (subst-all (make-simple-substitution vars args)
                type))
   (define (gen-serialize-type-rep me t->s)
     (match-define (user-defined-type-op vars type recursive?) me)
     `(user-defined-type-op (list ,@(for/list ([i (in-list vars)])
                                      `(quote ,i)))
                            ,(t->s type)
                            ,recursive?))])

(define (user-defined-type-constr? constr-rep)
  (match constr-rep
    [(struct* TypeConstructor ([real-trep-constr (? user-defined-type-op?)]))
     #t]
    [_ #f]))

(define (recursive-type-constr? constr)
  (match constr
    [(struct* TypeConstructor
              ([real-trep-constr (struct* user-defined-type-op ([recursive? recursive?]))]))
     recursive?]))
