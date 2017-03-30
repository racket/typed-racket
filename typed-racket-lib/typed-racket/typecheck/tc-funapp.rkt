#lang racket/base

(require (rename-in "../utils/utils.rkt" [infer r:infer])
         racket/match racket/list
         (prefix-in c: (contract-req))
         (env tvar-env)
         (for-syntax syntax/parse racket/base)
         (types utils subtype resolve abbrev
                substitute classes prop-ops)
         (typecheck tc-metafunctions tc-app-helper tc-subst)
         (rep type-rep)
         (r:infer infer))

(require-for-cond-contract syntax/stx)

(provide/cond-contract
  [tc/funapp
   (syntax? stx-list? Type? (c:listof tc-results1/c)
    (c:or/c #f tc-results/c)
    . c:-> . full-tc-results/c)])

;; macro that abstracts the common structure required to iterate over
;; the arrs of a polymorphic case lambda trying to infer the correct
;; substitution for the range when the inference algorithm succeeds
(define-syntax (for/first-valid-inference stx)
  (syntax-parse stx
    [(_  #:in-arrs [arr arrs-seq]
         #:arr-match match-expr
         #:function-syntax f-stx
         #:args-syntax args-stx
         #:infer-when when-expr
         #:maybe-inferred-substitution infer-expr
         #:function-type t
         #:args-results args-res
         #:expected expected)
     (syntax/loc stx
       (or (for/or ([arr arrs-seq])
             (match arr
               [match-expr
                #:when when-expr
                (let ([substitution infer-expr])
                  (and substitution
                       (tc/funapp1 f-stx args-stx (subst-all substitution arr)
                                   args-res expected #:check #f)))]
               [_ #f]))
           (poly-fail f-stx args-stx t args-res
                      #:name (and (identifier? f-stx) f-stx)
                      #:expected expected)))]))

(define (subst-dom-objs argtys argobjs rng)
  (subst-rep rng (for/list ([o (in-list argobjs)]
                            [t (in-list argtys)]
                            [idx (in-naturals)]
                            #:when (not (Empty? o)))
                   (list (cons 0 idx) o t))))

(define (tc/funapp f-stx args-stx f-type args-res expected)
  (match-define (list (tc-result1: argtys (PropSet: argps+ argps-) argobjs) ...) args-res)
  (define result
    (match f-type
      ;; we special-case this (no case-lambda) for improved error messages
      ;; tc/funapp1 currently cannot handle drest arities
      [(Function: (list (and a (arr: _ _ _ #f _))))
       (tc/funapp1 f-stx args-stx a args-res expected)]
      [(Function: arrs)
       ;; check there are no drests
       #:when (not (ormap arr-drest arrs))
       (cond
         ;; find the first function where the argument types match
         [(for/or ([a (in-list arrs)])
            (match a [(arr: dom _ rest _ _) (and (subtypes/varargs argtys dom rest) a)]))
          => (λ (a)
               ;; then typecheck here -- we call the separate function so that we get
               ;; the appropriate props/objects
               (tc/funapp1 f-stx args-stx a args-res expected #:check #f))]
         [else
          ;; if nothing matched, error
          (match arrs
            [(list (arr: doms rngs rests drests _) ...)
             (domain-mismatches
              f-stx args-stx f-type doms rests drests rngs args-res #f #f
              #:expected expected
              #:msg-thunk (lambda (dom)
                            (string-append
                             "No function domains matched in function application:\n"
                             dom)))])])]
      ;; any kind of dotted polymorphic function without mandatory keyword args
      [(PolyDots: (list fixed-vars ... dotted-var)
                  (Function: arrs))
       ;; check there are no mandatory kw args
       #:when (not (for*/or ([a (in-list arrs)]
                             [kws (in-value (arr-kws a))])
                     (ormap Keyword-required? kws)))
       (for/first-valid-inference
           #:in-arrs [a (in-list arrs)]
         #:arr-match (arr: dom rng rest drest _)
         #:function-syntax f-stx
         #:args-syntax args-stx
         #:infer-when
         ;; only try inference if the argument lengths are appropriate
         (cond [rest (<= (length dom) (length argtys))]
               [drest (and (<= (length dom) (length argtys))
                           (eq? dotted-var (cdr drest)))]
               [else (= (length dom) (length argtys))])
         ;; Only try to infer the free vars of the rng (which includes the vars
         ;; in props/objects).
         #:maybe-inferred-substitution
         (let ([rng (subst-dom-objs argtys argobjs rng)])
           (extend-tvars fixed-vars
                         (cond
                           [drest
                            (infer/dots
                             fixed-vars dotted-var argtys dom (car drest) rng (fv rng)
                             #:expected (and expected (tc-results->values expected)))]
                           [rest
                            (infer/vararg fixed-vars (list dotted-var) argtys dom rest rng
                                          (and expected (tc-results->values expected)))]
                           ;; no rest or drest
                           [else (infer fixed-vars (list dotted-var) argtys dom rng
                                        (and expected (tc-results->values expected)))])))
         #:function-type f-type
         #:args-results args-res
         #:expected expected)]
      ;; regular polymorphic functions without dotted rest, 
      ;; we do not choose any instantiations with mandatory keyword arguments
      [(Poly: vars (Function: arrs))
       ;; check there are no drests
       #:when (not (ormap arr-drest arrs))
       (for/first-valid-inference
           #:in-arrs [a (in-list arrs)]
         #:arr-match (arr: dom rng rest _ kws)
         #:function-syntax f-stx
         #:args-syntax args-stx
         ;; only try inference if the argument lengths are appropriate
         ;; and there's no mandatory kw
         #:infer-when
         (and (not (ormap Keyword-required? kws)) ((if rest <= =) (length dom) (length argtys)))
         ;; Only try to infer the free vars of the rng (which includes the vars
         ;; in props/objects).
         #:maybe-inferred-substitution
         (let ([rng (subst-dom-objs argtys argobjs rng)])
           (extend-tvars vars
                         (infer/vararg vars null argtys dom rest rng
                                       (and expected (tc-results->values expected)))))
         #:function-type f-type
         #:args-results args-res
         #:expected expected)]
      ;; Row polymorphism. For now we do really dumb inference that only works
      ;; in very restricted cases, but is probably enough for most cases in
      ;; the Racket codebase. Eventually this should be extended.
      [(PolyRow: vars constraints (and f-ty (Function/arrs: doms _ _ #f _)))
       (define (fail)
         (poly-fail f-stx args-stx f-type args-res
                    #:name (and (identifier? f-stx) f-stx)
                    #:expected expected))
       ;; there's only one row variable in a PolyRow (for now)
       (define row-var (car vars))
       ;; only infer if there is only one argument type that has the relevant
       ;; row type variable in its free variables in all cases
       (define row-var-idxs
         (for/list ([dom (in-list doms)])
           (define num-occurs
             (for/list ([dom-type dom]
                        [idx (in-naturals)]
                        #:when (member row-var (fv dom-type)))
               idx))
           (unless (<= (length num-occurs) 1)
             (fail))
           (if (null? num-occurs) 0 (car num-occurs))))
       (unless (or (< (length row-var-idxs) 2)
                   (apply = row-var-idxs))
         ;; row var wasn't in the same position in some cases
         (fail))
       (define idx (car row-var-idxs))
       (define resolved-argty (resolve (list-ref argtys idx)))
       (cond [(Class? resolved-argty)
              (define substitution
                (hash row-var
                      (t-subst (infer-row constraints resolved-argty))))
              (tc/funapp f-stx args-stx (subst-all substitution f-ty)
                         args-res expected)]
             [else (fail)])]
      ;; procedural structs
      [(Struct: _ _ _ (? Function? proc-ty) _ _)
       (tc/funapp f-stx #`(#,(syntax/loc f-stx dummy) . #,args-stx) proc-ty
                  (cons (ret f-type) args-res) expected)]
      ;; parameters are functions too
      [(Param: in out)
       (match argtys
         [(list) (ret out)]
         [(list t)
          (if (subtype t in)
              (ret -Void -true-propset)
              (tc-error/expr
               #:return (ret -Void -true-propset)
               "Wrong argument to parameter - expected ~a and got ~a"
               in t))]
         [_ (tc-error/expr
             "Wrong number of arguments to parameter - expected 0 or 1, got ~a"
             (length argtys))])]
      [(Distinction: _ _ t)
       (tc/funapp f-stx args-stx t args-res expected)]
      ;; resolve names, polymorphic apps, mu, etc
      [(? resolvable?)
       (tc/funapp f-stx args-stx (resolve-once f-type) args-res expected)]
      ;; a union of functions can be applied if we can apply all of the elements
      [(Union: (? Bottom?) ts) #:when (andmap Function? ts)
       (merge-tc-results
        (for/list ([fty (in-list ts)])
          (tc/funapp f-stx args-stx fty args-res expected)))]
      ;; bottom or error type is a perfectly good fcn type
      [(or (Bottom:) (Error:)) (ret f-type)]
      ;; otherwise fail
      [(Poly: ns (Function: arrs))
       (tc-error/expr
        (string-append "Cannot infer type instantiation for type ~a. Please add "
                       "more type annotations")
        f-type)]
      [_
       (tc-error/expr
        "Cannot apply expression of type ~a, since it is not a function type"
        f-type)]))
  ;; keep any info learned from the arguments
  (add-unconditional-prop result (apply -and (map -or argps+ argps-))))
