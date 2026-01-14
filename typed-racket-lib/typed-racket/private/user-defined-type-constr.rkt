#lang racket/base
(require "../rep/type-constr.rkt"
         racket/match
         racket/lazy-require)

;; lazy-require is necessary to break a module dependency cycle:
;; user-defined-type-constr.rkt -> substitute.rkt -> rep-utils.rkt ->
;; free-variance.rkt -> type-constr-env.rkt -> user-defined-type-constr.rkt
(lazy-require ["../types/substitute.rkt"
               (subst-all make-simple-substitution)])

(provide (struct-out user-defined-type-op)
         user-defined-type-constr?
         recursive-type-constr?)

;; poly-struct?: #t if this type constructor is for a polymorphic struct type
;; (used to prevent inlining during parsing, so App types are preserved for printing)
(struct user-defined-type-op (vars type recursive? poly-struct?) #:transparent
  #:methods gen:type-rep-maker
  [(define (gen-create-type-rep me args)
     (match-define (user-defined-type-op vars type recursive? _) me)
     (subst-all (make-simple-substitution vars args)
                type))
   (define (gen-serialize-type-rep me t->s)
     (match-define (user-defined-type-op vars type recursive? poly-struct?) me)
     `(user-defined-type-op (list ,@(for/list ([i (in-list vars)])
                                      `(quote ,i)))
                            ,(t->s type)
                            ,recursive?
                            ,poly-struct?))])

(define (user-defined-type-constr? constr-rep)
  (match constr-rep
    [(struct* TypeConstructor ([real-trep-constr (? user-defined-type-op?)]))
     #t]
    [_ #f]))

(define (recursive-type-constr? constr)
  (match-define (struct* TypeConstructor
                         ([real-trep-constr
                           (struct* user-defined-type-op ([recursive? recursive?]))]))
    constr)
  recursive?)
