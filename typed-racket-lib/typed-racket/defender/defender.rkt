#lang racket/base

;; TODO
;;  - [X] build + test occurrence-type optimizer
;;  - [ ] build/test erasure-racket, check T S E interactions + share types
;;
;; TODO
;; - [ ] need with-new-name-tables here?
;; - [ ] syntax-track-origin ? syntax/loc/track-origin ?

(require
  (only-in racket/format ~a)
  (only-in racket/set set-union)
  racket/match
  syntax/parse
  typed-racket/rep/type-rep
  typed-racket/rep/values-rep
  typed-racket/types/match-expanders
  (only-in typed-racket/optimizer/unboxed-let
    escapes?)
  (only-in typed-racket/private/type-annotation
    type-annotation
    get-type)
  (only-in typed-racket/env/transient-env
    transient-trusted-positive?)
  (only-in typed-racket/typecheck/internal-forms
    typed-struct
    typed-struct/exec)
  (only-in typed-racket/types/base-abbrev
    make-CyclicListof
    make-Listof)
  (only-in typed-racket/types/abbrev
    -Bottom
    -Void
    -String
    -Symbol
    -True
    -False
    ->
    -values)
  typed-racket/types/struct-table
  typed-racket/types/type-table
  typed-racket/types/union
  typed-racket/types/resolve
  typed-racket/types/utils
  (only-in typed-racket/private/syntax-properties
    type-ascription-property
    type-inst-property
    ignore^
    ignore-some^
    opt-lambda^
    kw-lambda^
    opt-lambda-property
    kw-lambda-property
    tr:class:def-property
    tr:class:name-table-property
    )
  (only-in racket/syntax
    format-id
    generate-temporary
    with-syntax*)
  (only-in syntax/srcloc
    build-source-location-list)
  (only-in (submod typed-racket/private/type-contract test-exports)
    has-contract-def-property?
    type->contract)
  (for-syntax
    racket/base)
  (for-template
    racket/base
    (only-in racket/contract/base any/c)
    racket/unsafe/ops
    (only-in racket/unsafe/undefined unsafe-undefined)
    typed-racket/types/numeric-predicates
    typed-racket/utils/transient-contract
    (only-in racket/private/class-internal find-method/who)
    (only-in typed-racket/private/class-literals class-internal)))

(provide defend-top)

(module+ test
  (require rackunit))

;; =============================================================================

(define (defend-top stx ctc-cache)
  (define rev-extra-def* (box '()))
  (define (register-extra-defs! ex*)
    (unless (null? ex*)
      (define stx (with-syntax ((ex* ex*)) #'(begin . ex*)))
      (set-box! rev-extra-def* (cons stx (unbox rev-extra-def*)))))
  (define defended-stx
    (let loop ([stx stx] [skip-dom? #f])
      (syntax-parse stx
        #:literals (#%plain-app #%plain-lambda begin case-lambda define-syntaxes define-values
                    find-method/who let-values letrec-values quote values)
        ;; unsound within exn-handlers^ ?
        [(let-values ([(_) _meth])
           (let-values ([(_) _rcvr])
             (let-values (((_) (#%plain-app find-method/who _ _ _)))
               (let-values ([(_) _args] ...) _))))
         ;; send (for objects), MUST come before ignored exprs
         (define tc-res (type-of stx (lambda () Univ)))
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
         (define class-name-table
           (car (trawl-for-property #'make-methods-body tr:class:name-table-property)))
         (define parse-info
           (syntax-parse class-name-table
            [tbl:internal-class-data
             (hash 'method-names
                   (set-union (syntax->datum #'tbl.public-externals)
                              (syntax->datum #'tbl.override-externals)
                              (syntax->datum #'tbl.augment-externals)
                              (syntax->datum #'tbl.pubment-externals))
                   'all-internal
                   (append (syntax->datum #'tbl.init-internals)
                           (syntax->datum #'tbl.init-field-internals)
                           (syntax->datum #'tbl.field-internals)
                           (syntax->datum #'tbl.public-internals)
                           (syntax->datum #'tbl.override-internals)
                           (syntax->datum #'tbl.inherit-internals)
                           (syntax->datum #'tbl.inherit-field-internals)
                           (syntax->datum #'tbl.pubment-internals)
                           (syntax->datum #'tbl.augment-internals))
                   'all-external
                   (append (syntax->datum #'tbl.init-externals)
                           (syntax->datum #'tbl.init-field-externals)
                           (syntax->datum #'tbl.field-externals)
                           (syntax->datum #'tbl.public-externals)
                           (syntax->datum #'tbl.override-externals)
                           (syntax->datum #'tbl.inherit-externals)
                           (syntax->datum #'tbl.inherit-field-externals)
                           (syntax->datum #'tbl.pubment-externals)
                           (syntax->datum #'tbl.augment-externals))
                   'private-names  (syntax->datum #'tbl.private-names)

                   ;'override-names (syntax->datum #'tbl.override-externals)
                   ;'pubment-names  (syntax->datum #'tbl.pubment-externals)
                   ;'augment-names  (syntax->datum #'tbl.augment-externals)
                   ;'type-parameters     type-parameters
                   ;'fresh-parameters    fresh-parameters
                   ;'superclass-expr     #'cls.superclass-expr
                   ;'make-methods        #'cls.make-methods
                   ;'initializer-self-id #'cls.initializer-self-id
                   ;'initializer-args-id #'cls.initializer-args-id
                   ;'initializer-body    #'cls.initializer-body
                   ;'optional-inits      (syntax->datum #'tbl.optional-inits)
                   ;'only-init-internals (syntax->datum #'tbl.init-internals)
                   ;'only-init-names     (syntax->datum #'tbl.init-externals)
                   ;;; the order of these names reflect the order in the class,
                   ;;; so use this list when retaining the order is important
                   ;'init-internals      (syntax->datum #'tbl.all-init-internals)
                   ;'init-rest-name     (and (attribute tbl.init-rest-name)
                   ;                         (syntax-e (attribute tbl.init-rest-name)))
                   ;'public-internals   (syntax->datum #'tbl.public-internals)
                   ;'override-internals (syntax->datum #'tbl.override-internals)
                   ;'pubment-internals  (syntax->datum #'tbl.pubment-internals)
                   ;'augment-internals  (syntax->datum #'tbl.augment-internals)
                   ;'method-internals
                   ;(set-union (syntax->datum #'tbl.public-internals)
                   ;           (syntax->datum #'tbl.override-internals))
                   ;'field-internals
                   ;(set-union (syntax->datum #'tbl.field-internals)
                   ;           (syntax->datum #'tbl.init-field-internals))
                   ;'inherit-internals
                   ;(syntax->datum #'tbl.inherit-internals)
                   ;'inherit-field-internals
                   ;(syntax->datum #'tbl.inherit-field-internals)
                   ;'init-names
                   ;(set-union (syntax->datum #'tbl.init-externals)
                   ;           (syntax->datum #'tbl.init-field-externals))
                   ;'field-names
                   ;(set-union (syntax->datum #'tbl.field-externals)
                   ;           (syntax->datum #'tbl.init-field-externals))
                   ;'public-names   (syntax->datum #'tbl.public-externals)
                   ;'inherit-names  (syntax->datum #'tbl.inherit-externals)
                   ;'inherit-field-names
                   ;(syntax->datum #'tbl.inherit-field-externals)
                   ;'private-names  (syntax->datum #'tbl.private-names)
                   ;'private-fields (syntax->datum #'tbl.private-field-names)
                   ;'overridable-names
                   ;(set-union (syntax->datum #'tbl.public-externals)
                   ;           (syntax->datum #'tbl.override-externals))
                   ;'augmentable-names
                   ;(set-union (syntax->datum #'tbl.pubment-externals)
                   ;           (syntax->datum #'tbl.augment-externals))
                   )]))
         (define internal-external-mapping
           (for/hash ([internal (hash-ref parse-info 'all-internal)]
                      [external (hash-ref parse-info 'all-external)])
             (values internal external)))
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
                        #,(let defend-method-def ([val #'make-methods-body])
                            (cond
                              [(pair? val)
                               (cons (defend-method-def (car val)) (defend-method-def (cdr val)))]
                              [(not (syntax? val))
                               val]
                              [(let ((name (tr:class:def-property val)))
                                 (and name (or (public-method-name? name)
                                               ;; TODO private = no dom check (right?)
                                               (private-method-name? name))))
                               (syntax-parse val
                                [((~literal #%plain-app)
                                  (~literal chaperone-procedure)
                                  ((~literal let-values) ((meth-id meth-fun)) let-body) . rest)
                                 ;; TODO custom defense here, avoid checking 1st arg to method? ... for keywords, meth-fun is not an immediate lambda
                                 (readd-props
                                   (quasisyntax/loc val
                                     (#%plain-app chaperone-procedure
                                       (let-values ((meth-id #,(loop #'meth-fun #f))) let-body) . rest))
                                   val)]
                                [_
                                  (raise-argument-error 'defend-method-def "tr:class:def-property #t" val)])]
                              [else
                               (define v (syntax-e val))
                               (if (pair? v)
                                 (readd-props
                                   (datum->syntax val (cons (defend-method-def (car v)) (defend-method-def (cdr v))))
                                   val)
                                 val)])))
                    #'make-methods-lambda)
                 (quote b) (quote #f)))
             stx))]
        ;; -- ignore -----------------------------------------------------------
        [_
         #:when (or (is-ignored? stx) ;; lookup in type-table's "ignored table"
                    (has-contract-def-property? stx))
         ;; TODO investigate ... contract-def should never be here right?
         stx]
        [(~or _:ignore^ _:ignore-some^) ;; struct def, class def ... not sure what else
         ;; TODO investigate ... can we loop & re-ignore? probably not!
         stx]
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
         (readd-props
           (quasisyntax/loc stx
             (let-values (((f-name)
                           (#%plain-lambda f-args
                             #,(let dom-check-loop ([f-body #'f-body]
                                                    [num-args num-args])
                                 (if (zero? num-args)
                                   (readd-props (loop f-body #f) f-body)
                                   (syntax-parse f-body
                                    #:literals (let-values if)
                                    [(let-values (((arg-id) (~and if-expr (if test default-expr arg)))) f-rest)
                                     ;; optional, default expression may need defense
                                     (define arg-ty (tc-results->type1 (type-of #'if-expr)))
                                     (define-values [ex* arg+] (protect-domain arg-ty #'arg (build-source-location-list f-body) ctc-cache))
                                     (void (register-extra-defs! ex*))
                                     (quasisyntax/loc f-body
                                       (let-values (((arg-id)
                                                     (if test
                                                       #,(syntax-parse #'test
                                                          [((~literal #%expression) ((~literal quote) #f))
                                                           #'default-expr]
                                                          [_
                                                           (readd-props (loop #'default-expr #f) #'default-expr)])
                                                       #,(if arg+ (readd-props arg+ #'arg) #'arg))))
                                         #,(dom-check-loop #'f-rest (- num-args 1))))]
                                    [(let-values (((arg-id) arg-val)) f-rest)
                                     ;; normal arg
                                     (define arg-ty (tc-results->type1 (type-of #'arg-val)))
                                     (define-values [ex* arg-val+] (protect-domain arg-ty #'arg-val (build-source-location-list f-body) ctc-cache))
                                     (void (register-extra-defs! ex*))
                                     (quasisyntax/loc f-body
                                       (let-values (((arg-id)
                                                     #,(if arg-val+ (readd-props arg-val+ #'arg-val) #'arg-val)))
                                         #,(dom-check-loop #'f-rest (- num-args 1))))]
                                    [_
                                     (raise-syntax-error 'defend-top "strange kw/opt function body"
                                                         stx f-body)]))))))
               #,(register-ignored #'body)))
             stx)]
        [((~or #%plain-lambda case-lambda) . _)
         #:when (not (maybe-type-of stx))
         stx]
        [((~literal lambda) formals . body)
         (raise-argument-error 'defend-top "what lambda" stx)]
        [(#%plain-lambda formals . body)
         ;; plain lambda
         (readd-props
           (quasisyntax/loc stx
             (#%plain-lambda formals .
               #,(let* ([body+
                          (readd-props (loop #'body #f) #'body)]
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
                                                 (raise-syntax-error 'defend-top "#%plain-lambda formals" #'formals args)])]
                                            [(check*)
                                             (let ((dom+
                                                   (for/fold ((acc '()))
                                                             ((dom (in-list dom*)))
                                                     (if (pair? dom) (cons (cdr dom) acc) acc))))
                                               (protect-loop rst dom+))]
                                            [(fst-ty)
                                             (if (type-annotation fst)
                                               (get-type fst #:default Univ)
                                               (apply Un
                                                      (for/fold ((acc '()))
                                                                ((dom (in-list dom*)))
                                                        (if (pair? dom) (cons (car dom) acc) acc))))]
                                            [(ex* fst+) (protect-domain fst-ty fst (build-source-location-list fst) ctc-cache)])
                                (void (register-extra-defs! ex*))
                                (if fst+ (cons fst+ check*) check*))))])
                   (if (null? check-formal*)
                     body+
                     (cons
                       (quasisyntax/loc #'body (#%plain-app void . #,check-formal*))
                       body+)))))
           stx)]
        [(case-lambda [formals* . body*] ...)
         ;; case-lambda, similar to lambda
         (define all-dom*
           (map Arrow-dom (syntax->arrows stx)))
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
                                      (readd-props (loop body #f) body)]
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
                                                              (raise-syntax-error 'defend-top "#%plain-lambda formals" formals args)])]
                                                         [(check*)
                                                          (let ((dom+
                                                                (for/fold ((acc '()))
                                                                          ((dom (in-list dom*)))
                                                                  (if (pair? dom) (cons (cdr dom) acc) acc))))
                                                            (protect-loop rst dom+))]
                                                         [(fst-ty)
                                                          (if (type-annotation fst)
                                                            (get-type fst #:default Univ)
                                                            (apply Un
                                                                   (for/fold ((acc '()))
                                                                             ((dom (in-list dom*)))
                                                                     (if (pair? dom) (cons (car dom) acc) acc))))]
                                                         [(ex* fst+) (protect-domain fst-ty fst (build-source-location-list fst) ctc-cache)])
                                             (void (register-extra-defs! ex*))
                                             (if fst+ (cons fst+ check*) check*))))])
                               (if (null? check-formal*)
                                 body+
                                 (cons
                                   (quasisyntax/loc body (#%plain-app void . #,check-formal*))
                                   body+)))])]))))
           stx)]
        [(#%plain-app (letrec-values (((a:id) e0)) b:id) e1* ...)
         #:when (free-identifier=? #'a #'b)
         ;; (for ....) combinators expand to a recursive function that does not escape,
         ;;  no need to check the domain --- use (loop e #true) to skip
         ;; TODO can the optimizer remove these checks instead?
         (define skip? (not (escapes? #'a #'e0 #false)))
         (with-syntax ((e0+ (readd-props (loop #'e0 skip?) #'e0))
                      ((e1*+ ...) (for/list ((e1 (in-list (syntax-e #'(e1* ...)))))
                                    (readd-props (loop e1 #f) e1))))
           (syntax/loc stx
             (#%plain-app (letrec-values (((a) e0+)) b) e1*+ ...))) ]
        [(x* ...)
         #:when (is-application? stx)
         (define stx+
           (readd-props
             (syntax*->syntax stx
               (for/list ([x (in-list (syntax-e #'(x* ...)))])
                 (readd-props (loop x #f) x)))
             stx))
         (define-values [pre* f post*] (split-application stx+))
         (cond
           [(or (is-ignored? f)
                (blessed-codomain? f)
                (cdr-list? f post*)
                (blessed-forloop-function? f))
            stx+]
           [else
            (define cod-tc-res (type-of stx))
            (define-values [extra* stx/cod]
              (protect-codomain cod-tc-res stx+ (build-source-location-list stx) ctc-cache))
            (void (register-extra-defs! extra*))
            (if stx/cod
              (readd-props stx/cod stx)
              stx+)])]
        [((~and x (~literal #%expression)) _)
         #:when (type-inst-property #'x)
         stx]
        [((~literal #%expression) e)
         #:when (type-ascription-property stx)
         (define e+ (readd-props (loop #'e #f) #'e))
         (define e++
           (with-syntax ([e+ e+])
             (syntax/loc stx (#%expression e+))))
         (readd-props e++ stx)]
        [_
         #:when (type-ascription-property stx)
         (raise-user-error 'defend-top "strange type-ascription ~a" (syntax->datum stx))]
        [(x* ...)
         (define stx+
           (syntax*->syntax stx
             (for/list ((x (in-list (syntax-e #'(x* ...)))))
               (readd-props (loop x #f) x))))
         (readd-props stx+ stx)]
        [_
         stx])))
  (values (reverse (unbox rev-extra-def*)) defended-stx))

;; copied
(define (trawl-for-property form accessor)
  (define (recur-on-all stx-list)
    (apply append (map (λ (stx) (trawl-for-property stx accessor))
                       (syntax->list stx-list))))
  (syntax-parse form
    #:literals (let-values letrec-values #%plain-app
                #%plain-lambda #%expression)
    [stx
     #:when (accessor #'stx)
     (list form)]
    [(let-values (b ...) body ...)
     (recur-on-all #'(b ... body ...))]
    ;; for letrecs, traverse the RHSs too
    [(letrec-values ([(x ...) rhs ...] ...) body ...)
     (recur-on-all #'(rhs ... ... body ...))]
    [(#%plain-app e ...)
     (recur-on-all #'(e ...))]
    [(#%plain-lambda (x ...) e ...)
     (recur-on-all #'(e ...))]
    [(#%expression e)
     (recur-on-all #'(e))]
    [_ '()]))

;; copied
(define-syntax-class internal-class-data
  #:literal-sets (kernel-literals)
  #:literals (class-internal values)
  (pattern (let-values ([() (begin (quote-syntax
                                    (class-internal
                                     (#:forall type-parameter:id ...)
                                     (#:all-inits all-init-names:id ...)
                                     (#:init init-names:name-pair ...)
                                     (#:init-field init-field-names:name-pair ...)
                                     (#:init-rest (~optional init-rest-name:id))
                                     (#:optional-init optional-names:id ...)
                                     (#:field field-names:name-pair ...)
                                     (#:public public-names:name-pair ...)
                                     (#:override override-names:name-pair ...)
                                     (#:private privates:id ...)
                                     (#:private-field private-fields:id ...)
                                     (#:inherit inherit-names:name-pair ...)
                                     (#:inherit-field inherit-field-names:name-pair ...)
                                     (#:augment augment-names:name-pair ...)
                                     (#:pubment pubment-names:name-pair ...))
                                    #:local)
                                   (#%plain-app values))])
             _)
           #:with type-parameters #'(type-parameter ...)
           #:with all-init-internals #'(all-init-names ...)
           #:with init-internals #'(init-names.internal ...)
           #:with init-externals #'(init-names.external ...)
           #:with init-field-internals #'(init-field-names.internal ...)
           #:with init-field-externals #'(init-field-names.external ...)
           #:with optional-inits #'(optional-names ...)
           #:with field-internals #'(field-names.internal ...)
           #:with field-externals #'(field-names.external ...)
           #:with public-internals #'(public-names.internal ...)
           #:with public-externals #'(public-names.external ...)
           #:with override-internals #'(override-names.internal ...)
           #:with override-externals #'(override-names.external ...)
           #:with inherit-externals #'(inherit-names.external ...)
           #:with inherit-internals #'(inherit-names.internal ...)
           #:with inherit-field-externals #'(inherit-field-names.external ...)
           #:with inherit-field-internals #'(inherit-field-names.internal ...)
           #:with augment-externals #'(augment-names.external ...)
           #:with augment-internals #'(augment-names.internal ...)
           #:with pubment-externals #'(pubment-names.external ...)
           #:with pubment-internals #'(pubment-names.internal ...)
           #:with private-names #'(privates ...)
           #:with private-field-names #'(private-fields ...)))

;; copied
(define-syntax-class name-pair
  (pattern (internal:id external:id)))

(define (readd-props! new-stx old-stx)
  (maybe-add-typeof-expr new-stx old-stx)
  (maybe-add-test-position new-stx old-stx)
  (maybe-register-ignored new-stx old-stx)
  (void))

(define (readd-props new-stx old-stx)
  (readd-props! new-stx old-stx)
  new-stx)

(define (register-ignored stx)
  (register-ignored! stx)
  stx)

(define maybe-type-of
  (let ((fail (lambda () #false)))
    (lambda (e) (type-of e fail))))

(define (maybe-add-typeof-expr new-stx old-stx)
  (let ((old-type (maybe-type-of old-stx)))
    (when old-type
      (add-typeof-expr new-stx old-type))))

(define (maybe-add-test-position new-stx old-stx)
  (maybe-add-test-true new-stx old-stx)
  (maybe-add-test-false new-stx old-stx)
  (void))

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

;; -----------------------------------------------------------------------------

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
     [(or (Poly: _ b)
          (PolyDots: _ b))
      (loop b)]
     [(Refine: parent pred)
      (raise-user-error 'refine "~s~n ~s~n ~s~n" ty parent pred)]
     [(DepFun: _ _ _)
      ty]
     [_
      (raise-arguments-error 'syntax->arrow-type "failed to parse arrow from type of syntax object"
        "e" (syntax->datum stx)
        "stx" stx
        "type" ty)])))

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

(define (blessed-codomain? stx)
  (if (identifier? stx)
    (or (syntax-property stx 'constructor-for) ;; 2020-03: could register in env/lexical-env instead
        ;; 2020-03 : struct predicates handled earlier in type checker
        (transient-trusted-positive? stx)
        ;; 2020-04 : TR ids are not always sound, see tests
        #;(and (typed-racket-identifier? stx)
             (not (struct-accessor? stx))
             (not (from-require/typed? stx))))
    (literal-function stx)))

(define (cdr-list?  f post*)
  ;; TODO put this in optimizer? / reuse optimizer?
  (define f-depth
    (and (identifier? f)
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
           [else #f])))
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
           #false]))))

(define (module-path-index-join* . x*)
  (let loop ((x* x*))
    (if (null? (cdr x*))
      (module-path-index-join (car x*) #f)
      (module-path-index-join (car x*) (loop (cdr x*))))))

;; Special case: no cod-check on for loop index functions
;; This may lead to unsoundness, but I don't know how else to allow
;;  (for ((x (open-input-port "aaa"))) ....)
;; Changing the type of `make-sequence` does not seem promising because the
;;  interesting parts are the return types.
(define blessed-forloop-function?
  (let ((for:mpi (module-path-index-join* "for.rkt" "pre-base.rkt" "private/base.rkt" 'racket/base)))
    (lambda (stx)
      (and (equal? (syntax-source-module stx) for:mpi)
           (let ((id (syntax-e stx)))
             (or (eq? id 'pos->vals)
                 (eq? id 'for-loop)))))))

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
            (#%plain-app transient-assert dom ctc 'ty-str 'ctx))))))
  (values extra-def* dom-stx+))

;; protect-codomain : ???
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
                                #'(#%plain-app transient-assert v ctc 'ty-str 'ctx)))
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

(define (syntax*->syntax ctx stx*)
  ;; TODO this may be breaking structure of input stx objects
  (datum->syntax ctx
    (if (null? stx*)
      '()
      (cons (car stx*) (syntax*->syntax ctx (cdr stx*))))))

(define (type->flat-contract t ctc-cache)
  (cond
    [(or (eq? t Univ)
         ;; TODO what's correct for unsafe-undef?
         #;(eq? unsafe-undefined (match t [(Val-able: v) v] [_ #f])))
     (values '() #f)]
    [else
     (define (fail #:reason r)
       (raise-user-error 'type->flat-contract "failed to convert type ~a to flat contract because ~a" t r))
     (match-define (list defs ctc)
       (type->contract t fail #:typed-side #false #:cache ctc-cache))
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

