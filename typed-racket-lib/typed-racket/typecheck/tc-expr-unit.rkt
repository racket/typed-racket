#lang racket/unit


(require (rename-in "../utils/utils.rkt" [private private-in])
         racket/match (prefix-in - (contract-req))
         "signatures.rkt"
         "check-below.rkt" "../types/kw-types.rkt"
         (types utils abbrev union subtype type-table path-type
                filter-ops remove-intersect resolve generalize)
         (private-in syntax-properties)
         (rep type-rep filter-rep object-rep)
         (only-in (infer infer) restrict)
         (utils tc-utils)
         (env lexical-env)
         racket/list
         racket/private/class-internal
         syntax/parse
         (only-in racket/list split-at)
         (typecheck internal-forms tc-envops)
         racket/sequence
         racket/extflonum
         ;; Needed for current implementation of typechecking letrec-syntax+values
         (for-template (only-in racket/base letrec-values)
                       ;; see tc-app-contracts.rkt
                       racket/contract/private/provide)

         (for-label (only-in '#%paramz [parameterization-key pz:pk])
                    (only-in racket/private/class-internal find-method/who)))

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
  (define-values (alias-path alias-id)
    (match obj
      [(Path: p x) (values p x)]
      [(Empty:) (values (list) id*)]))
  ;; calculate the type, resolving aliasing and paths if necessary
  (define ty (path-type alias-path (lookup-type/lexical alias-id)))
  
  (ret ty
       (if (overlap ty (-val #f))
           (-FS (-not-filter (-val #f) obj) (-filter (-val #f) obj))
           -true-filter)
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
  (--> syntax? Type/c tc-results/c)
  (tc-expr/check form (ret expected)))

(define (tc-expr/check form expected)
  (parameterize ([current-orig-stx form])
    ;; the argument must be syntax
    (unless (syntax? form)
      (int-err "bad form input to tc-expr: ~a" form))
    ;; typecheck form
    (define t (tc-expr/check/internal form expected))
    (add-typeof-expr form t)
    (cond-check-below t expected)))

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
  (cond [(and (identifier? var) (lookup-type/lexical var #:fail (λ _ #f)))
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
    ;(printf "form: ~a\n" (syntax-object->datum form))
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
      [(quote #f) (ret (-val #f) -false-filter)]
      [(quote #t) (ret (-val #t) -true-filter)]
      [(quote val)
       (match expected
         [(tc-result1: t)
          (ret (tc-literal #'val t) -true-filter)]
         [_
          (ret (tc-literal #'val) -true-filter)])]
      ;; syntax
      [(quote-syntax datum . _)
       (define expected-type
         (match expected
           [(tc-result1: t) t]
           [_ #f]))
       (ret (find-stx-type #'datum expected-type) -true-filter)]
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
      [(#%plain-app . _) (tc/app form expected)]
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
         (tc-body/check #'es (tc-any-results -top)))]
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
          (ret f -true-filter)]
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
      [(let-values ([(name ...) expr] ...) . body)
       (tc/let-values #'((name ...) ...) #'(expr ...) #'body expected)]
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
  (match (syntax->list body)
    [(list es ... e-final)
     ;; First, typecheck all the forms whose results are discarded.
     ;; If any one those causes the rest to be unreachable (e.g. `exit' or `error`,
     ;; then mark the rest as ignored.
     (let loop ([es es])
       (cond [(empty? es) ; Done, typecheck the return form.
              (tc-expr/check e-final expected)]
             [else
              ;; Typecheck the first form.
              (define e (first es))
              (define results (tc-expr/check e (tc-any-results -no-filter)))
              (define props
                (match results
                  [(tc-any-results: f) (list f)]
                  [(tc-results: _ (list (FilterSet: f+ f-) ...) _)
                   (map -or f+ f-)]
                  [(tc-results: _ (list (FilterSet: f+ f-) ...) _ _ _)
                   (map -or f+ f-)]))
              (with-lexical-env/extend-props
               props
               ;; If `e` bails out, mark the rest as ignored.
               #:unreachable (for ([x (in-list (cons e-final (rest es)))])
                               (register-ignored! x))
               ;; Keep going with an environment extended with the propositions that are
               ;; true if execution reaches this point.
               (loop (rest es)))]))]))

;; find-stx-type : Any [(or/c Type/c #f)] -> Type/c
;; recursively find the type of either a syntax object or the result of syntax-e
(define (find-stx-type datum-stx [expected #f])
  (match datum-stx
    [(? syntax? (app syntax-e stx-e))
     (match (and expected (resolve (restrict expected (-Syntax Univ) 'orig)))
       [(Syntax: t) (-Syntax (find-stx-type stx-e t))]
       [_ (-Syntax (find-stx-type stx-e))])]
    [(or (? symbol?) (? null?) (? number?) (? extflonum?) (? boolean?) (? string?) (? char?)
         (? bytes?) (? regexp?) (? byte-regexp?) (? keyword?))
     (tc-literal #`#,datum-stx expected)]
    [(cons car cdr)
     (match (and expected (resolve (restrict expected (-pair Univ Univ) 'orig)))
       [(Pair: car-t cdr-t) (-pair (find-stx-type car car-t) (find-stx-type cdr cdr-t))]
       [_ (-pair (find-stx-type car) (find-stx-type cdr))])]
    [(vector xs ...)
     (match (and expected (resolve (restrict expected -VectorTop 'orig)))
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
                    [t (in-sequence-forever (in-list ts) #f)])
           (cond-check-below (find-stx-type x t) t)))]
       [_ (make-HeterogeneousVector (for/list ([x (in-list xs)])
                                      (generalize (find-stx-type x #f))))])]
    [(box x)
     (match (and expected (resolve (restrict expected -BoxTop 'orig)))
       [(Box: t) (-box (check-below (find-stx-type x t) t))]
       [_ (-box (generalize (find-stx-type x)))])]
    [(? hash? h)
     (match (and expected (resolve (restrict expected -HashTop 'orig)))
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
