
#lang racket/unit

(require "signatures.rkt"
         "utils.rkt"
         syntax/parse syntax/stx racket/match racket/set
         "../signatures.rkt"
         "../tc-app-helper.rkt"
         "../tc-funapp.rkt"
         "../tc-metafunctions.rkt"
         "../../types/abbrev.rkt"
         "../../types/utils.rkt"
         "../../types/substitute.rkt"
         "../../types/subtype.rkt"
         "../../types/type-table.rkt"
         "../../rep/type-rep.rkt"
         "../../utils/tc-utils.rkt"
         "../../infer/infer.rkt"
         (for-label racket/base)
         ;; adjusted -1 since it's provided for-syntax
         (only-in (for-template racket/contract/private/provide)
                  contract-neg-party-property))


(import tc-expr^)
(export tc-app-keywords^)

(define-literal-set keyword-literals #:for-label (list))

(define-tc/app-syntax-class (tc/app-keywords expected)
  #:literal-sets (kernel-literals keyword-literals)
  (pattern (~and form
                 ((#%plain-app cpce s-kp fn kpe kws num)
                  kw-list
                  (#%plain-app list . kw-arg-list)
                  . *pos-args))
    #:declare cpce (id-from 'checked-procedure-check-and-extract 'racket/private/kw)
    #:declare s-kp (id-from 'struct:keyword-procedure 'racket/private/kw)
    #:declare kpe  (id-from 'keyword-procedure-extract 'racket/private/kw)
    ;; If this application is of a module boundary contract function or not
    ;; If #t, then the contract system has inserted an extra argument which we
    ;; need to ignore
    #:attr boundary-ctc? (contract-neg-party-property #'fn)
    #:do [(for-each register-ignored! (syntax->list #'form))] ; no type info, so can't optimize
    #:with pos-args (if (attribute boundary-ctc?)
                        (stx-cdr #'*pos-args)
                        #'*pos-args)

    (let ()
      (define (tc/app-mono-fun arrows)
        (tc-keywords #'(#%plain-app . form) arrows (type->list (tc-expr/t #'kws))
                     #'kw-arg-list #'pos-args expected))

      (define (tc/app-poly-fun vars arrow fail)
        (match-define (and ar (Arrow: dom rst kw-formals rng)) arrow)
        ;; if the types of the keyword arguments have type variables or rst is
        ;; set, stop.
        (unless (or (set-empty? (fv/list kw-formals)) (not rst))
          (fail))
        (match (stx-map single-value #'pos-args)
           [(list (tc-result1: argtys-t) ...)
            (let* ([subst (infer vars null argtys-t dom rng
                                 (and expected (tc-results->values expected)))])
              (unless subst
                (fail))
              (tc-keywords #'form (list (subst-all subst ar))
                           (type->list (tc-expr/t #'kws)) #'kw-arg-list #'pos-args expected))]))

      (match (tc-expr/t #'fn)
        [(Poly: vars
                (Fun: (list arrow)))
         (=> fail)
         (tc/app-poly-fun vars arrow fail)]
        [(Fun: arrows)
         (tc/app-mono-fun arrows)]
        [(Poly: _ (Fun: _))
         (tc-error/expr "Inference for polymorphic keyword functions not supported")]
        [(Intersection: (HasArrows: arrows) _)
         (tc/app-mono-fun arrows)]
        [(Intersection: (Poly: vars
                               (Fun: (list arrow))) _)
         (=> fail)
         (tc/app-poly-fun vars arrow fail)]
        [t
         (tc-error/expr "Cannot apply expression of type ~a, since it is not a function type" t)]))))

(define (tc-keywords/internal arity kws kw-args error?)
  (match arity
    [(Arrow: dom (not (? RestDots?)) ktys rng)
     ;; assumes that everything is in sorted order
     (let loop ([actual-kws kws]
                [actuals (stx-map tc-expr/t kw-args)]
                [formals ktys])
       (match* (actual-kws formals)
         [('() '())
          (void)]
         [(_ '())
          (if error?
              (tc-error/delayed "Unexpected keyword argument ~a" (car actual-kws))
              #f)]
         [('() (cons fst rst))
          (match fst
            [(Keyword: k _ #t)
             (if error?
                 (tc-error/delayed "Missing keyword argument ~a" k)
                 #f)]
            [_ (loop actual-kws actuals rst)])]
         [((cons k kws-rest) (cons (Keyword: k* t req?) form-rest))
          (cond [(eq? k k*) ;; we have a match
                 (if (subtype (car actuals) t)
                     ;; success
                     (loop kws-rest (cdr actuals) form-rest)
                     ;; failure
                     (and error?
                          (tc-error/delayed
                           "Wrong function argument type, expected ~a, got ~a for keyword argument ~a"
                           t (car actuals) k)
                          (loop kws-rest (cdr actuals) form-rest)))]
                [req? ;; this keyword argument was required
                 (if error?
                     (begin (tc-error/delayed "Missing keyword argument ~a" k*)
                            (loop kws-rest (cdr actuals) form-rest))
                     #f)]
                [else ;; otherwise, ignore this formal param, and continue
                 (loop actual-kws actuals form-rest)])]))]))

(define (tc-keywords form arrows kws kw-args pos-args expected)
  (match arrows
    [(list (and a (Arrow: dom (and rst (not (? RestDots?))) ktys rng rng-T+)))
     (tc-keywords/internal a kws kw-args #t)
     (tc/funapp (car (syntax-e form)) pos-args
                (->* dom rst rng :T+ rng-T+)
                (stx-map tc-expr pos-args) expected)]
    [(list (and a (Arrow: doms (and rsts (not (? RestDots?))) _ rngs rngs-T+)) ...)
     (let ([new-arrows
            (for/list ([a (in-list arrows)]
                       ;; find all the arrows where the keywords match
                       #:when (tc-keywords/internal a kws kw-args #f))
              (match a
                [(Arrow: dom (and rst (not (? RestDots?))) ktys rng rng-T+)
                 (make-Arrow dom rst '() rng rng-T+)]))])
       (if (null? new-arrows)
           (domain-mismatches
            (car (syntax-e form)) (cdr (syntax-e form))
            (make-Fun arrows) doms rsts rngs
            (stx-map tc-expr pos-args)
            #f #f #:expected expected
            #:msg-thunk
            (lambda (dom)
              (string-append "No function domains matched in function application:\n"
                             dom)))
           (tc/funapp (car (syntax-e form)) pos-args
                      (make-Fun new-arrows)
                      (stx-map tc-expr pos-args) expected)))]))

(define (type->list t)
  (match t
    [(Pair: (Value: (? keyword? k)) b)
     (cons k (type->list b))]
    [(? Base:Null?) null]
    [_ (int-err "bad value in type->list: ~a" t)]))
