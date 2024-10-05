#lang racket/base

(require (for-syntax racket/base
                     racket/syntax
                     syntax/parse)
         (for-template racket/base
                       racket/list
                       racket/unsafe/ops)
         racket/match
         syntax/parse
         (only-in "../utils/tc-utils.rkt" current-type-enforcement-mode)
         (only-in "../types/match-expanders.rkt" Listof:)
         "../optimizer/logging.rkt"
         "../optimizer/utils.rkt"
         "../rep/type-rep.rkt"
         "../typecheck/typechecker.rkt"
         "../types/base-abbrev.rkt"
         "../types/resolve.rkt"
         "../types/subtype.rkt"
         "../types/type-table.rkt"
         "../types/utils.rkt"
         "../utils/utils.rkt"
         "logging.rkt"
         "utils.rkt")

(provide pair-opt-expr)

(define-unsafe-syntax-class car)
(define-unsafe-syntax-class cdr)
(define-unsafe-syntax-class mcar)
(define-unsafe-syntax-class mcdr)
(define-unsafe-syntax-class set-mcar!)
(define-unsafe-syntax-class set-mcdr!)


(define-merged-syntax-class pair-op (car^ cdr^))
(define-merged-syntax-class mpair-op (mcar^ mcdr^ set-mcar!^ set-mcdr!^))


(define (has-pair-type? e)
  (subtypeof? e (-pair Univ Univ)))
;; can't do the above for mpairs, as they are invariant
(define (has-mpair-type? e)
  (match (maybe-type-of e) ; type of the operand
    [(tc-result1: (MPair: _ _)) #t]
    [_ #f]))

(define (log-pair-missed-opt stx irritant)
  (log-missed-optimization
   "car/cdr on a potentially empty list"
   "According to its type, the highlighted list could be empty. Access to it cannot be safely optimized. To fix this, restrict the type to non-empty lists, maybe by wrapping this expression in a check for non-emptiness."
   stx irritant))

(define-syntax-rule (log-pair-opt)
  (log-opt "pair" "Pair check elimination."))

(define-syntax-class pair-opt-expr
  #:commit
  #:attributes (opt)
  #:literal-sets (kernel-literals)

  ;; no logging here, redundant with actual pair opt
  (pattern :pair-derived-opt-expr)
  (pattern (#%plain-app op:pair-op p:opt-expr)
    #:when (or (has-pair-type? #'p)
               ;; in this case, we have a potentially empty list, but
               ;; it has to be a list, otherwise, there would have been
               ;; a type error
               (begin (log-pair-missed-opt this-syntax #'p) #f))
    #:do [(log-pair-opt)]
    #:with opt #'(op.unsafe p.opt))
  (pattern (#%plain-app op:mpair-op p:opt-expr e:opt-expr ...)
    #:when (or (has-mpair-type? #'p)
               (begin (log-pair-missed-opt this-syntax #'p) #f))
    #:do [(log-pair-opt)]
    #:with opt #'(op.unsafe p.opt e.opt ...)))


;; change the source location of a given syntax object
(define ((relocate loc-stx) stx)
  (datum->syntax stx (syntax->datum stx) loc-stx stx))

;; if the equivalent sequence of cars and cdrs is guaranteed not to fail,
;; we can optimize

(define-syntax gen-pair-derived-expr
  (syntax-parser
    [(_ name:id (orig:id seq ...) ...)
     (define/with-syntax (syntax-class-name ...) (generate-temporaries #'(orig ...)))
     (define/with-syntax (lit-class-name ...) (generate-temporaries #'(orig ...)))
     #'(begin
         (begin
           (define-literal-syntax-class lit-class-name (orig))
           (define-syntax-class syntax-class-name
             #:commit
             #:attributes (arg alt)
             (pattern (#%plain-app (~var op lit-class-name) arg)
               #:with alt (map (relocate #'op) (list seq ...))))) ...
         (define-merged-syntax-class name (syntax-class-name ...)))]))

(gen-pair-derived-expr pair-derived-expr
 (caar #'car #'car)
 (cadr #'car #'cdr)
 (cdar #'cdr #'car)
 (cddr #'cdr #'cdr)
 (caaar #'car #'car #'car)
 (caadr #'car #'car #'cdr)
 (cadar #'car #'cdr #'car)
 (caddr #'car #'cdr #'cdr)
 (cdaar #'cdr #'car #'car)
 (cdadr #'cdr #'car #'cdr)
 (cddar #'cdr #'cdr #'car)
 (cdddr #'cdr #'cdr #'cdr)
 (caaaar #'car #'car #'car #'car)
 (caaadr #'car #'car #'car #'cdr)
 (caadar #'car #'car #'cdr #'car)
 (caaddr #'car #'car #'cdr #'cdr)
 (cadaar #'car #'cdr #'car #'car)
 (cadadr #'car #'cdr #'car #'cdr)
 (caddar #'car #'cdr #'cdr #'car)
 (cadddr #'car #'cdr #'cdr #'cdr)
 (cdaaar #'cdr #'car #'car #'car)
 (cdaadr #'cdr #'car #'car #'cdr)
 (cdadar #'cdr #'car #'cdr #'car)
 (cdaddr #'cdr #'car #'cdr #'cdr)
 (cddaar #'cdr #'cdr #'car #'car)
 (cddadr #'cdr #'cdr #'car #'cdr)
 (cdddar #'cdr #'cdr #'cdr #'car)
 (cddddr #'cdr #'cdr #'cdr #'cdr)
 (first   #'car)
 (rest    #'cdr)
 (second  #'car #'cdr)
 (third   #'car #'cdr #'cdr)
 (fourth  #'car #'cdr #'cdr #'cdr)
 (fifth   #'car #'cdr #'cdr #'cdr #'cdr)
 (sixth   #'car #'cdr #'cdr #'cdr #'cdr #'cdr)
 (seventh #'car #'cdr #'cdr #'cdr #'cdr #'cdr #'cdr)
 (eighth  #'car #'cdr #'cdr #'cdr #'cdr #'cdr #'cdr #'cdr)
 (ninth   #'car #'cdr #'cdr #'cdr #'cdr #'cdr #'cdr #'cdr #'cdr)
 (tenth   #'car #'cdr #'cdr #'cdr #'cdr #'cdr #'cdr #'cdr #'cdr #'cdr))

(define-syntax-class pair-derived-opt-expr
  #:commit
  (pattern e:pair-derived-expr
    #:with opt
    ;; optimize alt inside-out, as long as it's safe to
    (let-values
        ([(t res)
          (for/fold ([t   (match (type-of #'e.arg)
                            [(tc-result1: t) (static-type->dynamic-type t)])]
                     [res #'e.arg])
              ([accessor (in-list (reverse (syntax->list #'e.alt)))])
            (cond
             [(and t (subtype t (-pair Univ Univ))) ; safe to optimize this one layer
              (syntax-parse accessor
                [op:pair-op
                 (log-pair-opt)
                 (values
                  (match (resolve t)
                    [(Pair: a d) ; peel off one layer of the type
                     (syntax-parse #'op
                       [:car^ a]
                       [:cdr^ d])]
                    [_ ; not a pair type, give up on optimizing more
                     #f])
                  #`(op.unsafe #,res))])]
             [else ; unsafe, just rebuild the rest of the accessors
              (log-pair-missed-opt accessor #'e.arg)
              (values t ; stays unsafe from now on
                      #`(#,accessor #,res))]))])
      res)))

(define (static-type->dynamic-type type)
  ;; simple: forget all type structure except list spines
  (define te-mode (current-type-enforcement-mode))
  (case te-mode
    ((deep)
     type)
    ((shallow)
     (match type
      [(Listof: _)
       (-lst Univ)]
      [(Pair: _ t-cdr)
       (let cdr-loop ((t t-cdr))
         (match t
          [(Pair: _ t-cdr)
           (-pair Univ (cdr-loop t-cdr))]
          [tail
           (-pair Univ (if (eq? tail -Null) -Null Univ))]))]
      [(app resolve (Pair: _ _))
       (-pair Univ Univ)]
      [_
        Univ]))
    (else (raise-optimizer-context-error te-mode))))
