#lang racket/base

;; Rewrite typed code to defend itself with shape checks
;; - typed functions check their inputs
;; - elimination forms (e.g. car) check their results

(provide shallow-rewrite-top)

(require
  racket/match
  (only-in racket/format ~a)
  (only-in racket/set set-union)
  (only-in racket/list flatten)
  (only-in racket/syntax generate-temporary with-syntax*)
  (only-in syntax/srcloc build-source-location-list)
  syntax/id-set
  syntax/parse
  typed-racket/env/index-env
  typed-racket/env/scoped-tvar-env
  typed-racket/env/tvar-env
  typed-racket/rep/type-rep
  typed-racket/rep/values-rep
  typed-racket/types/abbrev
  typed-racket/types/match-expanders
  typed-racket/types/resolve
  typed-racket/types/struct-table
  typed-racket/types/substitute
  typed-racket/types/type-table
  typed-racket/types/union
  typed-racket/types/utils
  typed-racket/utils/class-utils
  typed-racket/utils/plambda-utils
  typed-racket/utils/shallow-utils
  (only-in typed-racket/optimizer/unboxed-let escapes?)
  (only-in typed-racket/private/type-annotation type-annotation get-type)
  (only-in typed-racket/private/syntax-properties
    type-ascription-property
    type-inst-property
    plambda-property
    ignore^
    ignore-some^
    opt-lambda^
    kw-lambda^
    opt-lambda-property
    kw-lambda-property
    tr:class:def-property
    tr:class:name-table-property)
  (only-in (submod typed-racket/private/type-contract test-exports)
    type->contract)
  (for-syntax
    racket/base)
  (for-template
    racket/base
    (only-in racket/contract/base any/c) ;; for free-id test, to avoid rewrites
    racket/unsafe/ops
    (only-in racket/unsafe/undefined unsafe-undefined)
    typed-racket/types/numeric-predicates
    typed-racket/utils/shallow-contract
    (only-in racket/private/class-internal find-method/who)
    (only-in typed-racket/private/class-literals class-internal)))

;; =============================================================================

(define (shallow-rewrite-top stx ctc-cache)
  (define rev-extra-def* (box '()))
  (define (register-extra-defs! ex*)
    (unless (null? ex*)
      (define stx (with-syntax ((ex* ex*)) #'(begin . ex*)))
      (set-box! rev-extra-def* (cons stx (unbox rev-extra-def*)))))
  (define rewritten-stx
    (let loop ([stx stx] [skip-dom? #f] [trusted-fn* (immutable-free-id-set)])
      (syntax-parse stx
        #:literals (#%plain-app #%plain-lambda begin case-lambda define-syntaxes define-values
                    find-method/who let-values letrec-values quote values)

        [(let-values ([(_) _meth])
           (let-values ([(_) _rcvr])
             (let-values (((_) (#%plain-app find-method/who _ _ _)))
               (let-values ([(_) _args] ...) _))))
         ;; send (for objects)
         (define tc-res (maybe-type-of stx))
         (define-values [extra* stx/check]
           (protect-codomain tc-res stx (build-source-location-list stx) ctc-cache))
         (void (register-extra-defs! extra*))
         (if stx/check
           (register-ignored (readd-props stx/check stx))
           stx)]
        [(#%plain-app
           compose-class:id name:expr superclass:expr interface:expr internal:expr ...
           (~and make-methods-lambda (#%plain-lambda (local-accessor:id local-mutator:id local-method-or-field:id ...) make-methods-body))
           (quote b:boolean) (quote #f))
         ;; class def, see typecheck/check-class-unit
         (define parse-info
           (let ((name-table (car (trawl-for-property #'make-methods-body tr:class:name-table-property))))
             (parse-internal-class-data name-table)))
         (define internal-external-mapping (make-internal-external-mapping parse-info))
         (define public-method-name?
           (let ([name* (hash-ref parse-info 'method-names)])
             (lambda (name-stx)
               (memq (hash-ref internal-external-mapping (syntax-e name-stx) #f) name*))))
         (define private-method-name?
           (let ([name* (hash-ref parse-info 'private-names)])
             (lambda (name-stx)
               (memq (syntax-e name-stx) name*))))
         (register-ignored
           (readd-props
             (quasisyntax/loc stx
               (#%plain-app compose-class name superclass interface internal ...
                #,(readd-props
                    #`(#%plain-lambda (local-accessor local-mutator local-method-or-field ...)
                        #,(let shallow-rewrite-method-def ([val #'make-methods-body])
                            (cond
                              [(pair? val)
                               (cons (shallow-rewrite-method-def (car val)) (shallow-rewrite-method-def (cdr val)))]
                              [(not (syntax? val))
                               val]
                              [(let ((name (tr:class:def-property val)))
                                 (and name
                                      (or (public-method-name? name)
                                          (private-method-name? name))
                                      name))
                               => (lambda (val-name)
                                    (syntax-parse val
                                     [((~literal #%plain-app)
                                       (~literal chaperone-procedure)
                                       ((~literal let-values) ((meth-id meth-fun)) let-body) . rest)
                                      ;; TODO custom defense here, avoid checking 1st arg to method? ... for keywords, meth-fun is not an immediate lambda
                                      (readd-props
                                        (quasisyntax/loc val
                                          (#%plain-app chaperone-procedure
                                            (let-values ((meth-id #,(loop #'meth-fun (private-method-name? val-name) trusted-fn*))) let-body) . rest))
                                        val)]
                                     [_
                                       (raise-argument-error 'shallow-rewrite-method-def "tr:class:def-property #t" val)]))]
                              [else
                               (define v (syntax-e val))
                               (if (pair? v)
                                 (readd-props
                                   (datum->syntax val (cons (shallow-rewrite-method-def (car v)) (shallow-rewrite-method-def (cdr v))))
                                   val)
                                 val)])))
                    #'make-methods-lambda)
                 (quote b) (quote #f)))
             stx))]
        [((~or (~literal #%provide)
               (~literal #%require)
               (~literal begin-for-syntax)
               (~literal define-syntaxes)
               (~literal module*)
               (~literal module)
               (~literal quote)
               (~literal quote-syntax)) . _)
         stx]
        [(~and (~or :opt-lambda^ :kw-lambda^)
               (let-values (((f-name) (#%plain-lambda f-args f-body))) body))
         ;; opt/kw function
         (define num-args (length (syntax->list #'f-args)))
         (with-extended-env (get-poly-tvarss stx)
           (lambda ()
             (readd-props
               (quasisyntax/loc stx
                 (let-values (((f-name)
                               (#%plain-lambda f-args
                                 #,(let dom-check-loop ([f-body #'f-body]
                                                        [num-args num-args])
                                     (if (zero? num-args)
                                       (readd-props (loop f-body #f trusted-fn*) f-body)
                                       (syntax-parse f-body
                                        #:literals (let-values if)
                                        [(let-values (((arg-id) (~and if-expr (if test default-expr arg)))) f-rest)
                                         ;; optional, default expression may need defense
                                         (define arg-ty (tc-results->type1 (type-of #'if-expr)))
                                         (define-values [ex* arg+]
                                           (if skip-dom?
                                             (values '() #f)
                                             (protect-domain arg-ty #'arg (build-source-location-list f-body) ctc-cache)))
                                         (void (register-extra-defs! ex*))
                                         (quasisyntax/loc f-body
                                           (let-values (((arg-id)
                                                         (if test
                                                           #,(syntax-parse #'test
                                                              [((~literal #%expression) ((~literal quote) #f))
                                                               #'default-expr]
                                                              [_
                                                               (readd-props (loop #'default-expr #f trusted-fn*) #'default-expr)])
                                                           #,(if arg+ (readd-props arg+ #'arg) #'arg))))
                                             #,(dom-check-loop #'f-rest (- num-args 1))))]
                                        [(let-values (((arg-id) arg-val)) f-rest)
                                         ;; normal arg
                                         (define arg-ty (tc-results->type1 (type-of #'arg-val)))
                                         (define-values [ex* arg-val+]
                                           (if skip-dom?
                                             (values '() #f)
                                             (protect-domain arg-ty #'arg-val (build-source-location-list f-body) ctc-cache)))
                                         (void (register-extra-defs! ex*))
                                         (quasisyntax/loc f-body
                                           (let-values (((arg-id)
                                                         #,(if arg-val+ (readd-props arg-val+ #'arg-val) #'arg-val)))
                                             #,(dom-check-loop #'f-rest (- num-args 1))))]
                                        [_
                                         (raise-syntax-error 'shallow-rewrite-top "strange kw/opt function body"
                                                             stx f-body)]))))))
                   #,(register-ignored #'body)))
                 stx)))]
        [((~and lam-id (~or #%plain-lambda case-lambda)) formals . body)
         #:when (not (maybe-type-of stx))
         (define body+ (readd-props (loop #'body #f trusted-fn*) #'body))
         (readd-props
           (quasisyntax/loc stx
             (lam-id formals . #,body+))
           stx)]
        [(#%plain-lambda formals . body)
         (with-extended-env (get-poly-tvarss stx)
          (lambda ()
           (readd-props
             (quasisyntax/loc stx
               (#%plain-lambda formals .
                 #,(let* ([body+
                            (readd-props (loop #'body #f trusted-fn*) #'body)]
                          [dom* (map Arrow-dom (syntax->arrows stx))]
                          [check-formal*
                            (let protect-loop ([args #'formals]
                                               [dom* dom*])
                              (if (or (identifier? args)
                                      (null? args)
                                      (and (syntax? args) (null? (syntax-e args))))
                                '()
                                (let*-values ([(fst rst)
                                               (cond
                                                 [(pair? args)
                                                  (values (car args) (cdr args))]
                                                 [(syntax? args)
                                                  (let ((e (syntax-e args)))
                                                    (values (car e) (cdr e)))]
                                                 [else
                                                   (raise-syntax-error 'shallow-rewrite-top "#%plain-lambda formals" #'formals args)])]
                                              [(check*)
                                               (let ((dom+
                                                     (for/fold ((acc '()))
                                                               ((dom (in-list dom*)))
                                                       (if (pair? dom) (cons (cdr dom) acc) acc))))
                                                 (protect-loop rst dom+))]
                                              [(fst-ty)
                                               (let ((ann-ty (and (type-annotation fst #:infer #f) (get-type fst #:infer #t #:default Univ))))
                                                 (if (and ann-ty (not (Error? ann-ty)))
                                                   ann-ty
                                                   (apply Un (for/list ((dom (in-list dom*)) #:when (pair? dom)) (car dom)))))]
                                              [(ex* fst+)
                                               (if skip-dom?
                                                 (values '() #f)
                                                 (protect-domain fst-ty fst (build-source-location-list fst) ctc-cache))])
                                  (void (register-extra-defs! ex*))
                                  (if fst+ (cons fst+ check*) check*))))])
                     (if (null? check-formal*)
                       body+
                       (cons
                         (quasisyntax/loc #'body (#%plain-app void . #,check-formal*))
                         body+)))))
             stx)))]
        [(case-lambda [formals* . body*] ...)
         (define all-dom* (map Arrow-dom (syntax->arrows stx)))
         (with-extended-env (get-poly-tvarss stx)
           (lambda ()
             (readd-props
               (quasisyntax/loc stx
                 (case-lambda .
                     #,(for/list ([formals (in-list (syntax-e #'(formals* ...)))]
                                  [body (in-list (syntax-e #'(body* ...)))])
                         (cond
                           [(dead-lambda-branch? formals)
                            ;; no type
                            (quasisyntax/loc formals [#,formals . #,body])]
                           [else
                             (define matching-dom*
                               (let ([len (formals-length formals)])
                                 (for/list ((dom (in-list all-dom*))
                                            #:when (= len (length dom)))
                                   dom)))
                             (quasisyntax/loc stx
                               [#,formals .
                                #,(let* ([body+
                                          (readd-props (loop body #f trusted-fn*) body)]
                                         [check-formal*
                                           (let protect-loop ([args formals]
                                                              [dom* matching-dom*])
                                             (if (or (identifier? args)
                                                     (null? args)
                                                     (and (syntax? args) (null? (syntax-e args))))
                                               '()
                                               (let*-values ([(fst rst)
                                                              (cond
                                                                [(pair? args)
                                                                 (values (car args) (cdr args))]
                                                                [(syntax? args)
                                                                 (let ((e (syntax-e args)))
                                                                   (values (car e) (cdr e)))]
                                                                [else
                                                                  (raise-syntax-error 'shallow-rewrite-top "#%plain-lambda formals" formals args)])]
                                                             [(check*)
                                                              (let ((dom+
                                                                    (for/fold ((acc '()))
                                                                              ((dom (in-list dom*)))
                                                                      (if (pair? dom) (cons (cdr dom) acc) acc))))
                                                                (protect-loop rst dom+))]
                                                             [(fst-ty)
                                                              (if (type-annotation fst #:infer #f)
                                                                (get-type fst #:infer #t #:default Univ)
                                                                (apply Un
                                                                       (for/fold ((acc '()))
                                                                                 ((dom (in-list dom*)))
                                                                         (if (pair? dom) (cons (car dom) acc) acc))))]
                                                             [(ex* fst+)
                                                              (if skip-dom?
                                                                (values '() #f)
                                                                (protect-domain fst-ty fst (build-source-location-list fst) ctc-cache))])
                                                 (void (register-extra-defs! ex*))
                                                 (if fst+ (cons fst+ check*) check*))))])
                                   (if (null? check-formal*)
                                     body+
                                     (cons
                                       (quasisyntax/loc body (#%plain-app void . #,check-formal*))
                                       body+)))])]))))
               stx)))]
        [(#%plain-app (letrec-values (((a:id) e0)) b:id) e1* ...)
         #:when (free-identifier=? #'a #'b)
         ;; (for ....) combinators expand to a recursive function that does not escape,
         ;;  no need to check the domain --- use (loop e #true trusted-fn*) to skip
         (define skip? (not (escapes? #'a #'e0 #false)))
         (with-syntax ((e0+ (readd-props (loop #'e0 skip? (free-id-set-add trusted-fn* #'a)) #'e0))
                      ((e1*+ ...) (for/list ((e1 (in-list (syntax-e #'(e1* ...)))))
                                    (readd-props (loop e1 #f trusted-fn*) e1))))
           (syntax/loc stx
             (#%plain-app (letrec-values (((a) e0+)) b) e1*+ ...))) ]
        [(x* ...)
         #:when (is-application? stx)
         (define stx+
           (readd-props
             (syntax*->syntax stx
               (for/list ([x (in-list (syntax-e #'(x* ...)))])
                 (readd-props (loop x skip-dom? trusted-fn*) x)))
             stx))
         (define-values [pre* f post*] (split-application stx+))
         (cond
           [(or (is-ignored? f)
                (parameter-set-app? f post*)
                (trusted-codomain? f)
                (and (identifier? f)
                     (or (free-id-set-member? trusted-fn* f)
                         (cdr-list? f post*))))
            stx+]
           [else
            (define cod-tc-res (type-of stx))
            (define-values [extra* stx/cod]
              (protect-codomain cod-tc-res stx+ (build-source-location-list stx) ctc-cache))
            (void (register-extra-defs! extra*))
            (if stx/cod
              (readd-props stx/cod stx)
              stx+)])]
        [((~and x (~literal #%expression)) e)
         #:when (or (type-inst-property #'x)
                    (type-ascription-property stx))
         (define e+ (readd-props (loop #'e skip-dom? trusted-fn*) #'e))
         (define e++
           (with-syntax ([e+ e+])
             (syntax/loc stx (x e+))))
         (readd-props e++ stx)]
        [(x* ...)
         (define stx+
           (syntax*->syntax stx
             (for/list ((x (in-list (syntax-e #'(x* ...)))))
               (readd-props (loop x skip-dom? trusted-fn*) x))))
         (readd-props stx+ stx)]
        [_
         stx])))
  (values (reverse (unbox rev-extra-def*)) rewritten-stx))

(define (with-extended-env tvarss-list thunk)
  (define ns (flatten tvarss-list))
  (let outer ([tvarss-list tvarss-list])
    (if (or (null? tvarss-list)
            (null? (car tvarss-list)))
      (thunk)
      (let inner ([tvarss (get-poly-layer tvarss-list)])
        ;; loop copied from typecheck/tc-lambda-unit
        (match tvarss
          [(list) (outer (remove-poly-layer tvarss-list))]
          [(cons (list (list tvars ...) dotted) rest-tvarss)
           (extend-indexes dotted
              (extend-tvars/new tvars ns
                (inner rest-tvarss)))]
          [(cons tvars rest-tvarss)
           (extend-tvars/new tvars ns
             (inner rest-tvarss))])))))

(define (readd-props! new-stx old-stx)
  (maybe-add-typeof-expr new-stx old-stx)
  (maybe-add-test-position new-stx old-stx)
  (maybe-add-scoped-tvar new-stx old-stx)
  (maybe-register-ignored new-stx old-stx)
  (void))

(define (readd-props pre-stx old-stx)
  (define new-stx (copy-syntax-property* pre-stx old-stx))
  (readd-props! new-stx old-stx)
  new-stx)

(define (copy-syntax-property* new-stx old-stx)
  (for/fold ((new-stx new-stx))
            ((k (in-list (syntax-property-symbol-keys old-stx))))
    (syntax-property new-stx k (syntax-property old-stx k))))

(define (register-ignored stx)
  (register-ignored! stx)
  stx)

(define (maybe-add-typeof-expr new-stx old-stx)
  (let ((old-type (maybe-type-of old-stx)))
    (when old-type
      (add-typeof-expr new-stx old-type))))

(define (maybe-add-test-position new-stx old-stx)
  (maybe-add-test-true new-stx old-stx)
  (maybe-add-test-false new-stx old-stx)
  (void))

(define (maybe-add-scoped-tvar new-stx old-stx)
  (let ([old-layer (lookup-scoped-tvar-layer old-stx)])
    (when old-layer
      (add-scoped-tvars new-stx old-layer))))

(define (maybe-add-test-true new-stx old-stx)
  (when (test-position-takes-true-branch old-stx)
    (test-position-add-true new-stx))
  (void))

(define (maybe-add-test-false new-stx old-stx)
  (when (test-position-takes-false-branch old-stx)
    (test-position-add-false new-stx))
  (void))

(define (maybe-register-ignored new-stx old-stx)
  (when (is-ignored? old-stx)
    (register-ignored! new-stx))
  (void))

(define (formals-length stx)
  (formals-fold 0 (lambda (acc v) (add1 acc)) stx))

(define (formals-fold init f stx)
  (let loop ((v stx))
    (if (or (identifier? v)
            (null? v)
            (and (syntax? v) (null? (syntax-e v))))
      init
      (let*-values (((fst rst)
                     (cond
                       [(pair? v)
                        (values (car v) (cdr v))]
                       [(syntax? v)
                        (let ((e (syntax-e v)))
                          (values (car e) (cdr e)))]
                       [else
                         (raise-syntax-error 'formals-fold "lambda formals" stx)])))
        (f (loop rst) fst)))))

;; is-application? : Syntax -> Boolean
;; Returns #true if `stx` is a function application (an app that may need dynamic checking)
(define (is-application? stx)
  (syntax-parse stx
   [((~literal #%plain-app) . _)
    (has-type-annotation? stx)]
   [_
    #false]))

(define (has-type-annotation? x)
  (match (maybe-type-of x)
   [(tc-results: _ #f)
    ;; #f = don't handle rest dots TODO wait why not???? ... use  maybe-type-of only?
     #true]
   [_
     #false]))

;; split-application : Syntax -> (Values (Syntaxof List) Syntax (Syntaxof List))
(define (split-application stx)
  (syntax-parse stx
   #:literals (#%plain-app)
   #:datum-literals (apply)
   [((~and a #%plain-app) (~and b apply) f . arg*)
    (values #'(a b) #'f #'arg*)]
   [((~and a #%plain-app) f . arg*)
    (values #'(a) #'f #'arg*)]
   [_
    (raise-argument-error 'split-application "(Syntaxof App)" stx)]))

(define (syntax->arrows stx)
  (define raw-type (tc-results->type1 (type-of stx)))
  (let loop ([ty (and raw-type (normalize-type raw-type))])
    (match ty
     [(Fun: arrs)
      arrs]
     [(Union: _ ts)
      (apply append (map loop ts))]
     [(or (Poly: n* b)
          (PolyRow: n* b _))
      (loop (subst-all (make-simple-substitution n* (map make-F n*)) b))]
     [(PolyDots: (list n* ... n-dot) b)
      (define subst
        (make-simple-substitution n* (map make-F n*))
        ;; ben 2022-03-29: should we substitute the n-dot variable too? I would
        ;; think yes, but tests pass without and fail with (error = "substitute
        ;; used on ... variable")
        #;(hash-set (make-simple-substitution n* (map make-F n*))
                  n-dot (i-subst/dotted (list (make-F n-dot)) (make-F n-dot) n-dot)))
      (loop (subst-all subst b))]
     [(Refine: parent pred)
      (raise-user-error 'refine "~s~n ~s~n ~s~n" ty parent pred)]
     [(DepFun: _ _ _)
      ty]
     [_
      (raise-arguments-error 'syntax->arrow-type "failed to parse arrow from type of syntax object"
        "e" (syntax->datum stx)
        "stx" stx
        "type" ty)])))

(define (syntax*->syntax ctx stx*)
  (datum->syntax ctx
    (if (null? stx*)
      '()
      (cons (car stx*) (syntax*->syntax ctx (cdr stx*))))
    ctx ctx))

(define (tc-results->type* r)
  (match r
   [(tc-results: (list (tc-result: ts _ _) ...) #f)
    ts]
   [_
    #f]))

(define (tc-results->type1 r)
  (match r
   [(tc-result1: t)
    t]
   [_
    #f]))

(define (parameter-set-app? f args-stx)
  (define f-type (tc-results->type1 (maybe-type-of f)))
  (and
    f-type
    (Param? f-type)
    (not (null? (syntax-e args-stx)))))

(define (trusted-codomain? stx)
  (or
    (and (identifier? stx)
         (syntax-property stx 'constructor-for)) ;; 2020-03: could register in env/lexical-env instead
    (let ([cod-ty (tc-results->type1 (maybe-type-of stx))])
      ;; ? error when cod-ty is #f?
      (and cod-ty (shallow-trusted-positive? cod-ty)))
    (literal-function stx)))

;; cdr-list? : (-> identifier? _ _)
(define (cdr-list?  f post*)
  ;; TODO put this in optimizer? / reuse optimizer?
  (define f-depth
    (cond
      [(or (free-identifier=? f #'unsafe-cdr)
           (free-identifier=? f #'cdr))
       1]
      [(free-identifier=? f #'cddr)
       2]
      [(free-identifier=? f #'cdddr)
       3]
      [(free-identifier=? f #'cddddr)
       4]
      [else #f]))
  (define t
    (and f-depth
         (let ((e (syntax-e post*)))
           (and (pair? e) (tc-results->type1 (type-of (car e)))))))
  (and (Type? t)
       (let loop ((t t)
                  (d f-depth))
         (match t
          [(Listof: _)
           #true]
          [(Pair: _ t-cdr)
           (or (zero? d)
               (loop t-cdr (- d 1)))]
          [_
           (and (zero? d) (eq? t -Null))]))))

(define (module-path-index-join* . x*)
  (let loop ((x* x*))
    (if (null? (cdr x*))
      (module-path-index-join (car x*) #f)
      (module-path-index-join (car x*) (loop (cdr x*))))))

;; from-require/typed? : Identifier -> Boolean
;; Typed Racket adds this property to all require/typed identifiers,
;;  see `utils/require-contract.rkt`
(define (from-require/typed? stx)
  (syntax-property stx 'not-provide-all-defined))

(define (typed-racket-identifier? stx)
  (define ib (identifier-binding stx))
  (and (pair? ib)
       (or (identifier-binding-from-this-module? ib)
           (identifier-binding-from-typed-racket-module? ib))))

(define (identifier-binding-from-this-module? ib)
  (match ib
   [(list src-mpi _src-id nom-src-mpi nom-src-id 0 0 0)
    (and (equal? src-mpi (module-path-index-join #f #f))
         (equal? src-mpi nom-src-mpi))]
   [_
    #false]))

(define (identifier-binding-from-typed-racket-module? ib)
  (match ib
   [(list src-mpi _src-id _nom-src-mpi _nom-src-id 0 0 0)
    (typed-racket-mpi? src-mpi)]
   [_
    #false]))

(define typed-racket-mpi?
  (let ([cache (make-hash)])
    (λ (mpi)
      (hash-ref! cache mpi
        (λ () ;; Typed Racket always installs a `#%type-decl` submodule
          (let* ([mpi+ (module-path-index-join '(submod "." #%type-decl) mpi)])
            (parameterize ([current-namespace (make-base-namespace)])
              (with-handlers ([exn:fail:contract? (lambda (exn) #f)])
                (and mpi+
                     (dynamic-require mpi+ #f)
                     #t)))))))))

(define (protect-domain dom-type dom-stx ctx ctc-cache)
  (define-values [extra-def* ctc-stx]
    (if dom-type
      (type->flat-contract dom-type ctc-cache)
      (values '() #f)))
  (define dom-stx+
    (if (not ctc-stx)
      #f
      (with-syntax ([ctc ctc-stx]
                    [dom dom-stx]
                    [ty-str (format "~a" dom-type)]
                    [ctx ctx])
        (register-ignored
          (syntax/loc dom-stx
            (#%plain-app shallow-shape-check dom ctc 'ty-str 'ctx))))))
  (values extra-def* dom-stx+))

(define (protect-codomain cod-tc-res app-stx ctx ctc-cache)
  (define t* (tc-results->type* cod-tc-res))
  (cond
   [(or (not t*) (null? t*))
    (values '() #f)]
   [else
    (define-values [extra-def* ctc-stx*]
      (type->flat-contract* t* ctc-cache))
    (define cod-stx+
      (if (not (ormap values ctc-stx*))
        ;; Nothing to check
        #f
        ;; Assemble everything into a syntax object that:
        ;; - performs the application
        ;; - binds the result(s) to temporary variable(s)
        ;; - checks the tag of each temporary
        (with-syntax ([app app-stx])
          (define var-name 'dyn-cod)
          ;; - application returns +1 results:
          ;;   - bind all,
          ;;   - check the ones with matching contracts,
          ;;   - return all
          (with-syntax* ([v*
                          (for/list ([_t (in-list t*)])
                            (generate-temporary var-name))]
                         [(check-v* ...)
                          (for/list ((ctc-stx (in-list ctc-stx*))
                                     (type (in-list t*))
                                     (v-stx (in-list (syntax-e #'v*)))
                                     (i (in-naturals))
                                     #:when ctc-stx)
                            (define if-stx
                              (with-syntax ([ctc ctc-stx]
                                            [v v-stx]
                                            [ty-str (format "~a" type)]
                                            [ctx ctx])
                                #'(#%plain-app shallow-shape-check v ctc 'ty-str 'ctx)))
                            (register-ignored! if-stx)
                            if-stx)])
            (define new-stx
              (quasisyntax/loc app-stx
                (let-values ([v* app])
                  (begin check-v* ...  (#%plain-app values . v*)))))
            (void
              (add-typeof-expr new-stx cod-tc-res)
              (register-ignored! (caddr (syntax-e new-stx))))
            new-stx))))
    (values extra-def* cod-stx+)]))

(define (literal-function x)
  (syntax-parse x
   [((~or (~literal lambda)
          (~literal #%plain-lambda)
          (~literal case-lambda)) . _) #true]
   [_ #false]))

(define (type->flat-contract t ctc-cache)
  (cond
    [(eq? t Univ)
     (values '() #f)]
    [else
     (define (fail #:reason r)
       (raise-user-error 'type->flat-contract "failed to convert type ~a to flat contract because ~a" t r))
     (match-define (list defs ctc)
       (type->contract t fail #:typed-side 'both #:cache ctc-cache))
     (match t
      [(Refine: _ _)
       ;; do not lift defs; they may use a local var
       ;; e.g. (lambda (a) (lambda (b : (Refine ... a b ...)) ....))
       (define ctc+ (quasisyntax/loc ctc (let-values () #,@defs #,ctc)))
       (register-ignored! ctc+)
       (values '() ctc+)]
      [_
       (define ctc+ ;; type variables make an any/c, for example
         (if (free-identifier=? ctc #'any/c) #f ctc))
       (for-each register-ignored! defs)
       (values defs ctc+)])]))

(define (type->flat-contract* t* ctc-cache)
  (for/fold ((extra-def* '())
             (ctc-stx* '())
             #:result (values (reverse extra-def*) (reverse ctc-stx*)))
            ((t (in-list t*)))
    (define-values [ex* ctc-stx] (type->flat-contract t ctc-cache))
    (values (rev-append ex* extra-def*) (cons ctc-stx ctc-stx*))))

(define (rev-append a* b*)
  (let loop ((a* a*) (b* b*))
    (if (null? a*) b* (loop (cdr a*) (cons (car a*) b*)))))

