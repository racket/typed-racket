#lang racket/unit


(require (rename-in "../utils/utils.rkt" [private private-in])
         racket/match (prefix-in - (contract-req))
         "signatures.rkt"
         "check-below.rkt" "../types/kw-types.rkt"
         "integer-refinements.rkt"
         (types utils abbrev subtype type-table path-type
                prop-ops overlap resolve generalize tc-result
                numeric-tower)
         (private-in syntax-properties parse-type)
         (rep type-rep prop-rep object-rep)
         (only-in (infer infer) intersect)
         (utils tc-utils identifier)
         (env lexical-env scoped-tvar-env)
         racket/list
         racket/private/class-internal
         syntax/parse
         (typecheck internal-forms tc-envops)
         racket/sequence
         racket/extflonum
         ;; Needed for current implementation of typechecking letrec-syntax+values
         (for-template (only-in racket/base list letrec-values
                                + - * < <= = >= > add1 sub1 modulo
                                min max vector-length random
                                make-vector)
                       ;; see tc-app-contracts.rkt
                       racket/contract/private/provide)

         (for-label (only-in '#%paramz [parameterization-key pz:pk])
                    (only-in racket/private/class-internal find-method/who))
         (for-syntax racket/base racket/syntax))

(import tc-if^ tc-lambda^ tc-app^ tc-let^ tc-send^ check-subforms^ tc-literal^
        check-class^ check-unit^ tc-expression^)
(export tc-expr^)

(define-literal-set tc-expr-literals #:for-label
  (find-method/who))


;; typecheck an identifier
;; the identifier has variable effect
;; tc-id : identifier -> tc-results
(define/cond-contract (tc-id id)
  (--> identifier? full-tc-results/c)
  (define rename-id (contract-rename-id-property id))
  (define id* (or rename-id id))
  ;; see if id* is an alias for an object
  ;; if not (-id-path id*) is returned
  (define obj (lookup-alias/lexical id*))
  (define ty
    (match obj
      [(Empty:) (lookup-id-type/lexical id*)]
      [_ (lookup-obj-type/lexical obj)]))
  (ret ty
       (if (overlap? ty (-val #f))
           (-PS (-not-type obj (-val #f)) (-is-type obj (-val #f)))
           -true-propset)
       obj))

;; typecheck an expression, but throw away the effect
;; tc-expr/t : Expr -> Type
(define (tc-expr/t e)
  (match (single-value e)
    [(tc-result1: t _ _) t]
    [t (int-err "tc-expr returned ~a, not a single tc-result, for ~a" t (syntax->datum e))]))

(define (tc-expr/check/t e t)
  (match (tc-expr/check e t)
    [(tc-result1: t) t]))

;; typecheck an expression by passing tr-expr/check a tc-results
(define/cond-contract (tc-expr/check/type form expected)
  (--> syntax? Type? tc-results/c)
  (tc-expr/check form (ret expected)))

;; form : what expression are we typechecking?
;; expected : what is the expected tc-result (can be #f)
;; existential? : do we want to create an existential
;;   identifier for this expression if it does not
;;   have a non-trivual object? This is useful when
;;   the type of other expressions can depend on
;;   the specific type of this term.
(define (tc-expr/check form expected [existential? #f])
  (parameterize ([current-orig-stx form])
    ;; the argument must be syntax
    (unless (syntax? form)
      (int-err "bad form input to tc-expr: ~a" form))
    (define result
      ;; if there is an annotation, use that expected type for internal checking
      (syntax-parse form
        [exp:type-ascription^
         (add-scoped-tvars #'exp (parse-literal-alls (attribute exp.value)))
         (tc-expr/check/internal #'exp (parse-tc-results (attribute exp.value)))]
        [_ (reduce-tc-results/subsumption
            (tc-expr/check/internal form expected))]))
    ;; if 'existential?' is true, then it means this expression should be
    ;; given an existential identifier as an object if it has no object
    (define adjusted-result
      (cond
        [existential?
         (match result
           [(tc-result1: t ps (not (? Object?)))
            (ret t ps (-id-path (gen-existential-id)))]
           [_ result])]
        [else result]))
    (add-typeof-expr form adjusted-result)
    (cond-check-below adjusted-result expected)))

;; typecheck and return a truth value indicating a typecheck failure (#f)
;; or success (any non-#f value)
(define (tc-expr/check? form expected)
  (parameterize ([current-type-error? #f])
    (with-handlers ([exn:fail:syntax? (λ (_) #f)])
      (dynamic-wind
        (λ () (save-errors!))
        (λ ()
          (let ([result (tc-expr/check form expected)])
            (and (not (current-type-error?)) result)))
        (λ () (restore-errors!))))))

(define (tc-expr/check/t? form expected)
  (match (tc-expr/check? form expected)
    [(tc-result1: t) t]
    [#f #f]))

(define (explicit-fail stx msg var)
  (cond [(and (identifier? var) (lookup-id-type/lexical var #:fail (λ _ #f)))
         =>
         (λ (t)
           (tc-error/expr #:stx stx
                          (string-append (syntax-e msg) "; missing coverage of ~a")
                          t))]
         [else (tc-error/expr #:stx stx (syntax-e msg))]))

;; tc-expr/check/internal : syntax maybe[tc-results] -> tc-results
(define/cond-contract (tc-expr/check/internal form expected)
  (--> syntax? (-or/c tc-results/c #f) full-tc-results/c)
  (parameterize ([current-orig-stx form])
    ;; the argument must be syntax
    (unless (syntax? form)
      (int-err "bad form input to tc-expr: ~a" form))
    (syntax-parse form
      #:literal-sets (kernel-literals tc-expr-literals)
      ;; a TR-annotated class
      [stx:tr:class^
       (check-class form expected)]
      ;; Unit forms
      [stx:tr:unit^
       (check-unit form expected)]
      [stx:tr:unit:invoke^
       (check-invoke-unit form expected)]
      [stx:tr:unit:compound^
       (check-compound-unit form expected)]
      [stx:tr:unit:from-context^
       (check-unit-from-context form expected)]
      [stx:exn-handlers^
       (register-ignored! form)
       (check-subforms/with-handlers form expected) ]
      [(~and stx:typed-racket:ignore-type-information^ (#%expression inner))
       (tc-expr/check/internal #'inner Univ)
       (ret Univ -tt-propset -empty-obj)]
      ;; explicit failure
      [t:typecheck-failure
       (explicit-fail #'t.stx #'t.message #'t.var)]
      [t:assert-typecheck-failure
       (cond
         [(tc-expr/check? #'t.body expected)
          (tc-error/expr #:stx #'t.body (format "Expected a type check error!"))]
         [else expected])]
      ;; data
      [(quote #f) (ret (-val #f) -false-propset)]
      [(quote #t) (ret (-val #t) -true-propset)]
      [(quote val)
       (ret (match expected
              [(tc-result1: t) (tc-literal #'val t)]
              [_ (tc-literal #'val)])
            -true-propset
            ;; sometimes we want integer's symbolic objects
            ;; to be themselves
            (let ([v (syntax-e #'val)])
              (if (and (exact-integer? v)
                       (with-refinements?))
                  (-lexp v)
                  -empty-obj)))]
      ;; syntax
      [(quote-syntax datum . _)
       (define expected-type
         (match expected
           [(tc-result1: t) t]
           [_ #f]))
       (ret (find-stx-type #'datum expected-type) -true-propset)]
      ;; mutation!
      [(set! id val)
       (match-let* ([(tc-result1: id-t) (single-value #'id)]
                    [(tc-result1: val-t) (single-value #'val)])
         (unless (subtype val-t id-t)
           (type-mismatch id-t val-t "mutation only allowed with compatible types"))
         (ret -Void))]
      ;; top-level variable reference - occurs at top level
      [(#%top . id) (tc-id #'id)]
      [(#%variable-reference . _)
       (ret -Variable-Reference)]
      ;; identifiers
      [x:identifier (tc-id #'x)]
      ;; w-c-m
      [(with-continuation-mark e1 e2 e3)
       (define key-t (single-value #'e1))
       (match key-t
         [(tc-result1: (Continuation-Mark-Keyof: rhs))
          (tc-expr/check/type #'e2 rhs)
          (tc-expr/check #'e3 expected)]
         [(? (λ (result)
               (and (identifier? #'e1)
                    (free-identifier=? #'pz:pk #'e1 #f (syntax-local-phase-level)))))
          (tc-expr/check/type #'e2 Univ)
          (tc-expr/check #'e3 expected)]
         [(tc-result1: key-t)
          ;(check-below key-t -Symbol)
          ;; FIXME -- would need to protect `e2` with any-wrap/c contract
          ;; instead, just fail

          ;(tc-expr/check/type #'e2 Univ)
          ;(tc-expr/check #'e3 expected)
          (tc-error/expr "with-continuation-mark requires a continuation-mark-key, but got ~a" key-t)])]
      ;; application
      [(#%plain-app fun . args-stx)
       (define result (tc/app form expected))
       (cond
         [(and (identifier? #'fun)
               (with-refinements?))
          (maybe-add-linear-integer-refinements #'fun #'args-stx result)]
         [else result])]
      ;; #%expression
      [(#%expression e) (tc/#%expression form expected)]
      ;; begin
      [(begin . es)
       (tc-body/check #'es expected)]
      [(begin0 e)
       (tc-expr/check #'e expected)]
      [(begin0 e . es)
       (begin0
         (tc-expr/check #'e expected)
         (tc-body/check #'es (-tc-any-results -tt)))]
      ;; if
      [(if tst thn els) (tc/if-twoarm #'tst #'thn #'els expected)]
      ;; lambda
      [(#%plain-lambda formals . body)
       (tc/lambda form #'(formals) #'(body) expected)]
      [(case-lambda [formals . body] ...)
       (tc/lambda form #'(formals ...) #'(body ...) expected)]
      ;; send
      [(let-values ([(_) meth])
         (let-values ([(rcvr-var) rcvr])
           (let-values (((meth-var) (~and find-app (#%plain-app find-method/who _ _ _))))
             (let-values ([(arg-var) args] ...)
               (if wrapped-object-check
                   ignore-this-case
                   (~and core-app
                         (~or (#%plain-app _ _ _arg-var2 ...)
                              (let-values ([(_) _] ...)
                                (#%plain-app (#%plain-app _ _ _ _ _ _)
                                             _ _ _ ...)))))))))
       (register-ignored! form)
       (tc/send #'find-app #'core-app
                #'rcvr-var #'rcvr
                #'meth-var #'meth
                #'(arg-var ...) #'(args ...)
                expected)]
      ;; kw function def
      ;; TODO simplify this case
      [(~and (let-values ([(f) fun]) . body) kw:kw-lambda^)
       #:when expected
       (match expected
         [(tc-result1: (and f (or (? Fun?) (Poly-unsafe: _ (? Fun?)))))
          (define actual-kws (attribute kw.value))
          (check-kw-arity actual-kws f)
          (tc-expr/check/type #'fun (kw-convert f actual-kws #:split #t))
          (ret f -true-propset)]
         [_ (tc-expr/check form #f)])]
      ;; opt function def
      [(~and (let-values ([(f) fun]) . body) opt:opt-lambda^)
       #:when expected
       (define conv-type
         (match expected
           [(tc-result1: fun-type)
            (match-define (list required-pos optional-pos optional-supplied?)
                          (attribute opt.value))
            (opt-convert fun-type required-pos optional-pos optional-supplied?)]
           [_ #f]))
       (if conv-type
           (begin (tc-expr/check/type #'fun conv-type) expected)
           (tc-expr/check form #f))]
      [(~and _:kw-lambda^
         (let-values ([(f) fun])
           (let-values _
             (#%plain-app
              maker
              lambda-for-kws
              (case-lambda ; wrapper function
                (formals . cl-body) ...)
              (~or (quote (mand-kw:keyword ...))
                   (~and _ (~bind [(mand-kw 1) '()])))
              (quote (all-kw:keyword ...))
              . rst))))
       (define p (plambda-property form))
       (ret (kw-unconvert (tc-expr/t (plambda-property #'fun p))
                          (syntax->list #'(formals ...))
                          (syntax->datum #'(mand-kw ...))
                          (syntax->datum #'(all-kw ...))))]
      [(~and opt:opt-lambda^
             (let-values ([(f) fun])
               (case-lambda (formals . cl-body) ...)))
       (define p (plambda-property form))
       (ret (opt-unconvert (tc-expr/t (plambda-property #'fun p))
                           (syntax->list #'(formals ...))))]
      ;; let
      [(let-values bindings . body)
       (define bindings*
         (syntax-parse #'body
           #:literal-sets (kernel-literals)
           ;; special case: let-values that originates from an application of a
           ;; kw function. we may need to ignore contract-related arguments
           [((kw-app1 (kw-app2 cpce s-kp fn kpe kws num) ; see tc-app/tc-app-keywords.rkt
                      kw-list
                      (kw-app3 list . kw-arg-list)
                      . *pos-args))
            #:declare cpce (id-from 'checked-procedure-check-and-extract 'racket/private/kw)
            #:declare s-kp (id-from 'struct:keyword-procedure 'racket/private/kw)
            #:declare kpe  (id-from 'keyword-procedure-extract 'racket/private/kw)
            #:declare kw-app1  (id-from '#%app 'racket/private/kw)
            #:declare kw-app2  (id-from '#%app 'racket/private/kw)
            #:declare kw-app3  (id-from '#%app 'racket/private/kw)
            #:declare list (id-from 'list 'racket/private/kw)
            #:when (contract-neg-party-property #'fn) ; contracted
            ;; ignore the rhs which refers to a contract-lifted definition
            ;; this code may compute the negative blame party, which may involve
            ;; things that are not typecheckable
            (syntax-parse #'bindings
              [(c1 [(contract-lhs) contract-rhs] cs ...)
               ;; give up on optimizing the whole let, part of it is missing type info
               ;; (not that this expansion could be optimized anyway)
               (register-ignored! form)
               #'(c1 cs ...)]
              [_
               (int-err "malformed kw arg let-values ~a" #'bindings)])]
           [_ ; not the special case, leave bindings as is
            #'bindings]))
       (syntax-parse bindings*
         [([(name ...) expr] ...)
          (tc/let-values #'((name ...) ...) #'(expr ...) #'body expected)])]
      [(letrec-values ([(name ...) expr] ...) . body)
       (tc/letrec-values #'((name ...) ...) #'(expr ...) #'body expected)]
      ;; other
      [_ (int-err "cannot typecheck unknown form : ~s" (syntax->datum form))])))

;; type check form in the current type environment
;; if there is a type error in form, or if it has the wrong annotation, error
;; otherwise, produce the type of form
;; syntax[expr] -> type
(define (tc-expr form)
  (tc-expr/check form #f))

(define (single-value form [expected #f])
  (define t (tc-expr/check form expected))
  (match t
    [(tc-result1: _ _ _) t]
    [_ (tc-error/expr
          #:stx form
          "expected single value, got multiple (or zero) values")]))

(define (tc-dep-fun-arg form [expected #f])
  (define t (tc-expr/check form expected #t))
  (match t
    [(tc-result1: _ _ _) t]
    [_ (tc-error/expr
        #:stx form
        "expected single value, got multiple (or zero) values")]))


;; tc-body/check: syntax? tc-results? -> tc-results?
;; Body must be a non empty sequence of expressions to typecheck.
;; The final one will be checked against expected.
(define (tc-body/check body expected)
  (define any-res (-tc-any-results #f))
  (define exps (syntax->list body))
  (let loop ([exps exps])
    (match exps
      [(list tail-exp) (tc-expr/check tail-exp expected)]
      [(cons e rst)
       (define results (tc-expr/check e any-res))
       (define props
         (match results
           [(tc-any-results: p) (list p)]
           [(tc-results: tcrs _)
            (map (match-lambda
                   [(tc-result: _ (PropSet: p+ p-) _)
                    (-or p+ p-)])
                 tcrs)]))
       (with-lexical-env+props
         props
         #:expected any-res
         ;; If `e` bails out, mark the rest as ignored.
         #:unreachable (for-each register-ignored! rst)
         ;; Keep going with an environment extended with the
         ;; propositions that are true if execution reaches this
         ;; point.
         (loop rst))])))

;; find-stx-type : Any [(or/c Type? #f)] -> Type?
;; recursively find the type of either a syntax object or the result of syntax-e
(define (find-stx-type datum-stx-or-datum [expected-type #f])
  (match datum-stx-or-datum
    [(? syntax? (app syntax-e stx-e))
     (match (and expected-type (resolve (intersect expected-type (-Syntax Univ))))
       [(Syntax: t) (-Syntax (find-stx-type stx-e t))]
       [_ (-Syntax (find-stx-type stx-e))])]
    [(or (? symbol?) (? null?) (? number?) (? extflonum?) (? boolean?) (? string?) (? char?)
         (? bytes?) (? regexp?) (? byte-regexp?) (? keyword?))
     (tc-literal #`#,datum-stx-or-datum expected-type)]
    [(cons car cdr)
     (match (and expected-type (resolve (intersect expected-type (-pair Univ Univ))))
       [(Pair: car-t cdr-t) (-pair (find-stx-type car car-t) (find-stx-type cdr cdr-t))]
       [_ (-pair (find-stx-type car) (find-stx-type cdr))])]
    [(and (vector xs ...) v)
     (cond
      [(immutable? v)
       (match (and expected-type (resolve (intersect expected-type (-ivec Univ))))
        [(Immutable-Vector: t)
         (make-Immutable-Vector
          (check-below
           (apply Un
                  (for/list ([x (in-list xs)])
                    (find-stx-type x t)))
           t))]
        [(Immutable-HeterogeneousVector: ts)
         (make-Immutable-HeterogeneousVector
          (for/list ([x (in-list xs)]
                     [t (in-list/rest ts #f)])
            (cond-check-below (find-stx-type x t) t)))]
        [_
         (make-Immutable-HeterogeneousVector
          (for/list ([x (in-list xs)])
            (generalize (find-stx-type x #f))))])]
      [else
       (match (and expected-type (resolve (intersect expected-type -Mutable-VectorTop)))
        [(Mutable-Vector: t)
         (make-Mutable-Vector
          (check-below
           (apply Un
                  (for/list ([x (in-list xs)])
                    (find-stx-type x t)))
           t))]
        [(Mutable-HeterogeneousVector: ts)
         (make-Mutable-HeterogeneousVector
          (for/list ([x (in-list xs)]
                     [t (in-list/rest ts #f)])
            (cond-check-below (find-stx-type x t) t)))]
        [_
         (make-Mutable-HeterogeneousVector
          (for/list ([x (in-list xs)])
            (generalize (find-stx-type x #f))))])])]
    [(box x)
     (match (and expected-type (resolve (intersect expected-type -BoxTop)))
       [(Box: t) (-box (check-below (find-stx-type x t) t))]
       [_ (-box (generalize (find-stx-type x)))])]
    [(? hash? hash-val) (tc-hash find-stx-type hash-val expected-type)]
    [(? prefab-struct-key prefab-val) (tc-prefab find-stx-type prefab-val expected-type)]
    [_ Univ]))
