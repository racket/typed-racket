#lang racket/unit


(require (rename-in "../utils/utils.rkt" [private private-in])
         racket/match (prefix-in - (contract-req))
         "signatures.rkt"
         "check-below.rkt" "../types/kw-types.rkt"
         (types utils abbrev subtype type-table path-type
                prop-ops overlap resolve generalize tc-result
                numeric-tower)
         (private-in syntax-properties parse-type)
         (rep type-rep prop-rep object-rep)
         (only-in (infer infer) intersect)
         (utils tc-utils)
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
                                min max vector-length random)
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

(define (tc-expr/check form expected)
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
    (add-typeof-expr form result)
    (cond-check-below result expected)))

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
      ;; explicit failure
      [t:typecheck-failure
       (explicit-fail #'t.stx #'t.message #'t.var)]
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
                       (with-linear-integer-arithmetic?))
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
      [(#%plain-app . _)
       (define result (tc/app form expected))
       (cond
         [(with-linear-integer-arithmetic?)
          (add-applicable-linear-info form result)]
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
         (tc-body/check #'es (tc-any-results -tt)))]
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
         [(tc-result1: (and f (or (Function: _)
                                  (Poly: _ (Function: _)))))
          (define actual-kws (attribute kw.value))
          (check-kw-arity actual-kws f)
          (tc-expr/check/type #'fun (kw-convert f actual-kws #:split #t))
          (ret f -true-propset)]
         [(or (tc-results: _) (tc-any-results: _))
          (tc-expr/check form #f)])]
      ;; opt function def
      [(~and (let-values ([(f) fun]) . body) opt:opt-lambda^)
       #:when expected
       (define conv-type
         (match expected
           [(tc-result1: fun-type)
            (match-define (list required-pos optional-pos)
                          (attribute opt.value))
            (opt-convert fun-type required-pos optional-pos)]
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
       (ret (kw-unconvert (tc-expr/t #'fun)
                          (syntax->list #'(formals ...))
                          (syntax->datum #'(mand-kw ...))
                          (syntax->datum #'(all-kw ...))))]
      [(~and opt:opt-lambda^
             (let-values ([(f) fun])
               (case-lambda (formals . cl-body) ...)))
       (ret (opt-unconvert (tc-expr/t #'fun)
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


;; tc-body/check: syntax? tc-results? -> tc-results?
;; Body must be a non empty sequence of expressions to typecheck.
;; The final one will be checked against expected.
(define (tc-body/check body expected)
  (define any-res (tc-any-results #f))
  (define exps (syntax->list body))
  (let loop ([exps exps])
    (match exps
      [(list tail-exp) (tc-expr/check tail-exp expected)]
      [(cons e rst)
       (define results (tc-expr/check e any-res))
       (define props
         (match results
           [(tc-any-results: p) (list p)]
           [(tc-results: _ (list (PropSet: p+ p-) ...) _)
            (map -or p+ p-)]
           [(tc-results: _ (list (PropSet: p+ p-) ...) _ _ _)
            (map -or p+ p-)]))
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
(define (find-stx-type datum-stx [expected #f])
  (match datum-stx
    [(? syntax? (app syntax-e stx-e))
     (match (and expected (resolve (intersect expected (-Syntax Univ))))
       [(Syntax: t) (-Syntax (find-stx-type stx-e t))]
       [_ (-Syntax (find-stx-type stx-e))])]
    [(or (? symbol?) (? null?) (? number?) (? extflonum?) (? boolean?) (? string?) (? char?)
         (? bytes?) (? regexp?) (? byte-regexp?) (? keyword?))
     (tc-literal #`#,datum-stx expected)]
    [(cons car cdr)
     (match (and expected (resolve (intersect expected (-pair Univ Univ))))
       [(Pair: car-t cdr-t) (-pair (find-stx-type car car-t) (find-stx-type cdr cdr-t))]
       [_ (-pair (find-stx-type car) (find-stx-type cdr))])]
    [(vector xs ...)
     (match (and expected (resolve (intersect expected -VectorTop)))
       [(Vector: t)
        (make-Vector
         (check-below
          (apply Un
                 (for/list ([x (in-list xs)])
                   (find-stx-type x t)))
          t))]
       [(HeterogeneousVector: ts)
        (make-HeterogeneousVector
         (for/list ([x (in-list xs)]
                    [t (in-list/rest ts #f)])
           (cond-check-below (find-stx-type x t) t)))]
       [_ (make-HeterogeneousVector (for/list ([x (in-list xs)])
                                      (generalize (find-stx-type x #f))))])]
    [(box x)
     (match (and expected (resolve (intersect expected -BoxTop)))
       [(Box: t) (-box (check-below (find-stx-type x t) t))]
       [_ (-box (generalize (find-stx-type x)))])]
    [(? hash? h)
     (match (and expected (resolve (intersect expected -HashtableTop)))
       [(Hashtable: kt vt)
        (define kts (hash-map h (lambda (x y) (find-stx-type x kt))))
        (define vts (hash-map h (lambda (x y) (find-stx-type y vt))))
        (make-Hashtable
         (check-below (apply Un kts) kt)
         (check-below (apply Un vts) vt))]
       [_ (make-Hashtable (generalize (apply Un (map find-stx-type (hash-keys h))))
                          (generalize (apply Un (map find-stx-type (hash-values h)))))])]
    [(? prefab-struct-key)
     ;; FIXME is there a type for prefab structs?
     Univ]
    [_ Univ]))



;; adds linear info for the following operations:
;; + * < <= = >= >
;; when the arguments are integers w/ objects
;; a lot of the content in this function should eventually
;; just be moved to the actual types of the respective
;; racket identifiers, but in an effort to move progress forward
;; and not break programs currently typechecking, this more
;; explicit, hard-coded helper will do (i.e. some of these functions
;; currently have extremely large types in TR -- modifying their
;; type is not always trivial)
(define (add-applicable-linear-info form result)
  ;; class to recognize expressions that typecheck at a subtype of -Int
  ;; and whose object is non-empty
  (define-syntax-class (t/obj type)
    #:attributes (obj)
    (pattern e:expr
             #:do [(define o
                     (match (type-of #'e)
                       [(tc-result1: t _ (? Object? o))
                        #:when (subtype t type)
                        o]
                       [_ #f]))]
             #:fail-unless o (format "not a ~a expr with a non-empty object" type)
             #:attr obj o))
  (define-syntax (obj stx)
    (syntax-case stx ()
      [(_ e)
       (with-syntax ([e* (format-id #'e "~a.obj" (syntax->datum #'e))])
         (syntax/loc #'e (attribute e*)))]))
  ;; class to recognize int comparisons and associate their
  ;; internal TR prop constructors
  (define-syntax-class int-comparison
    #:attributes (constructor)
    (pattern (~literal <) #:attr constructor -lt)
    (pattern (~literal <=) #:attr constructor -leq)
    (pattern (~literal >=) #:attr constructor -geq)
    (pattern (~literal >) #:attr constructor -gt)
    (pattern (~literal =) #:attr constructor -eq))

  ;; takes a result and adds p to the then proposition
  ;; and (not p) to the else proposition
  (define (add-p/not-p result p)
    (match result
      [(tc-result1: t (PropSet: p+ p-) o)
       (ret t
            (-PS (-and p p+) (-and (negate-prop p) p-))
            o)]
      [_ result]))

  ;; takes a list of arguments to a comparison function
  ;; and returns the list of atomic facts that would hold
  ;; if returned #t
  (define (comparison-props comp obj1 obj2 obj-rest)
    (define-values (props _)
      (for/fold ([atoms (list (comp obj1 obj2))]
                 [prev-obj obj2])
                ([obj (in-list obj-rest)])
        (values (cons (-lt prev-obj obj) atoms)
                obj)))
    props)
  ;; inspect the function appplication to see if it is a linear
  ;; integer compatible form we want to enrich with info when
  ;; #:with-linear-integer-arithmetic is specified by the user
  (match result
    [(tc-result1: ret-t ps _)
     (syntax-parse form
       ;; *
       [(#%plain-app (~literal *) (~var e1 (t/obj -Int)) (~var es (t/obj -Int)) ...)
        (define product-obj (apply -obj* (obj e1) (obj es)))
        (cond
          [(Object? product-obj)
           (ret (-refine/fresh x ret-t (-eq (-lexp x) product-obj))
                ps
                product-obj)]
          [else result])]
       ;; +
       [(#%plain-app (~literal +) (~var e (t/obj -Int)) (~var es (t/obj -Int)) ...)
        (define l (apply -lexp (obj e) (obj es)))
        (ret (-refine/fresh x ret-t (-eq (-lexp x) l))
             ps
             l)]
       ;; -
       [(#%plain-app (~literal -) (~var e (t/obj -Int)) (~var es (t/obj -Int)) ...)
        (define l (apply -lexp
                         (obj e)
                         (for/list ([o (in-list (obj es))])
                           (scale-obj -1 o))))
        (ret (-refine/fresh x ret-t (-eq (-lexp x) l))
             ps
             l)]
       ;; add1
       [(#%plain-app (~literal add1) (~var e (t/obj -Int)))
        (define l (-lexp 1 (obj e)))
        (ret (-refine/fresh x ret-t (-eq (-lexp x) l))
             ps
             l)]
       ;; sub1
       [(#%plain-app (~literal sub1) (~var e (t/obj -Int)))
        (define l (-lexp -1 (obj e)))
        (ret (-refine/fresh x ret-t (-eq (-lexp x) l))
             ps
             l)]
       ;; modulo
       [(#%plain-app (~literal modulo) (~var e1 (t/obj -Int)) (~var e2 (t/obj -Nat)))
        (ret (-refine/fresh x ret-t (-lt (-lexp x) (obj e2)))
             ps
             -empty-obj)]
       ;; max
       [(#%plain-app (~literal max) (~var e1 (t/obj -Int)) (~var es (t/obj -Int)) ...)
        (ret (-refine/fresh x ret-t
                            (apply -and (let ([l (-lexp x)])
                                          (for/list ([o (in-list (cons (obj e1) (obj es)))])
                                            (-geq l o)))))
             ps
             -empty-obj)]
       ;; min
       [(#%plain-app (~literal min) (~var e1 (t/obj -Int)) (~var es (t/obj -Int)) ...)
        (ret (-refine/fresh x ret-t
                            (apply -and (let ([l (-lexp x)])
                                          (for/list ([o (in-list (cons (obj e1) (obj es)))])
                                            (-leq l o)))))
             ps
             -empty-obj)]
       ;; random
       [(#%plain-app (~literal random) (~var e1 (t/obj -Int)))
        (ret (-refine/fresh x ret-t (-lt (-lexp x) (obj e1)))
             ps
             -empty-obj)]
       [(#%plain-app (~literal random) (~var e1 (t/obj -Int)) (~var e2 (t/obj -Int)))
        (ret (-refine/fresh x ret-t (-and (-leq (obj e1) (-lexp x))
                                          (-lt (-lexp x) (obj e2))))
             ps
             -empty-obj)]
       ;; vector-length
       [(#%plain-app (~literal vector-length) (~var e1 (t/obj -VectorTop)))
        (define l (-vec-len-of (obj e1)))
        (ret (-refine/fresh x ret-t (-eq (-lexp x) l))
             ps
             l)]
       [(#%plain-app comp:int-comparison
                     (~var e1 (t/obj -Int))
                     (~var e2 (t/obj -Int))
                     (~var es (t/obj -Int)) ...)
        (define p (apply -and (comparison-props (attribute comp.constructor)
                                                (obj e1)
                                                (obj e2)
                                                (obj es))))
        (add-p/not-p result p)]
       [_ result])]
    [_ result]))
