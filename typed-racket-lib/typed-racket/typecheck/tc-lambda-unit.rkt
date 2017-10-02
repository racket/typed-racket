#lang racket/unit

(require "../utils/utils.rkt"
         racket/list syntax/parse syntax/stx
         racket/match syntax/private/id-table racket/set
         racket/sequence
         (contract-req)
         (rep type-rep object-rep rep-utils)
         (rename-in (types abbrev utils)
                    [-> t:->]
                    [->* t:->*]
                    [one-of/c t:one-of/c])
         (private type-annotation syntax-properties)
         (types resolve type-table)
         (typecheck signatures tc-metafunctions tc-subst)
         (env lexical-env tvar-env index-env scoped-tvar-env)
         (utils tc-utils)
         (for-template racket/base)
         (for-syntax racket/base))

(import tc-expr^)
(export tc-lambda^)


(define-syntax-class cl-rhs
  #:literal-sets (kernel-literals)
  #:attributes (i cond)
  [pattern i:id #:attr cond #f]
  [pattern (if cond:id i:id e:expr)])

(define-syntax-class rebuild-let*
  #:literal-sets (kernel-literals)
  #:attributes (mapping flag-mapping)
  (pattern (#%expression :rebuild-let*))
  (pattern (let-values ([(new-id) e:cl-rhs]) body:rebuild-let*)
           #:attr mapping (free-id-table-set (attribute body.mapping) #'e.i #'new-id)
           #:attr flag-mapping (if (attribute e.cond)
                                   (free-id-table-set (attribute body.flag-mapping) #'e.i #'e.cond)
                                   (attribute body.flag-mapping)))
  (pattern body:expr
           #:attr mapping (make-immutable-free-id-table)
           #:attr flag-mapping (make-immutable-free-id-table)))

(define (expected-str tys-len rst arg-len rest-id)
  (format "Expected function with ~a argument~a~a, but got function with ~a argument~a~a"
          tys-len
          (if (= tys-len 1) "" "s")
          (if rst
              " and a rest arg"
              "")
          arg-len
          (if (= arg-len 1) "" "s")
          (if rest-id " and a rest arg" "")))

;; tc-lambda-body: Typechecks the body with the given args and names and returns the resulting arr?.
;; arg-names: The identifiers of the positional args
;; arg-types: The types of the positional args
;; rest-arg+type: Either #f for no rest argument or (cons rest-id rest-type)
;;                where rest-id is the identifier of the rest arg,
;;                and (ListOf rest-type) is the type that identifier would
;;                have in the function body
;; expected: The expected type of the body forms.
;; body: The body of the lambda to typecheck.
(define/cond-contract
  (tc-lambda-body arg-names arg-types #:rest-arg+type [rest-arg+type #f] #:expected [expected #f] body)
  (->* ((listof identifier?) (listof Type?) syntax?)
       (#:rest-arg+type (or/c #f (cons/c identifier? (or/c Type? RestDots?)))
        #:expected (or/c #f tc-results/c))
       Arrow?)

  (define-values (rst-id rst-type names types)
    (match rest-arg+type
      [(cons id rst)
       (values id rst
               (cons id arg-names)
               (cons (match rst
                       [(? Bottom?) -Null]
                       [(? Type?) (-lst rst)]
                       [(RestDots: dty dbound)
                        (make-ListDots dty dbound)])
                     arg-types))]
      [_ (values #f #f arg-names arg-types)]))

  (-Arrow
   arg-types
   (abstract-results
    (with-extended-lexical-env
        [#:identifiers names
         #:types types]
      (tc-body/check body expected))
    arg-names #:rest-id rst-id)
   #:rest rst-type))

;; check-clause: Checks that a lambda clause has arguments and body matching the expected type
;; arg-list: The identifiers of the positional args in the lambda form
;; rest-id: The identifier of the rest arg, or #f for no rest arg
;; body: The body of the lambda to typecheck.
;; arg-tys: The expected positional argument types.
;; rst: #f, expected rest arg Type, or expected RestDots
;; ret-ty: The expected type of the body of the lambda.
(define/cond-contract (check-clause arg-list rest-id body arg-tys rst ret-ty)
  ((listof identifier?)
   (or/c #f identifier?) syntax? (listof Type?) (or/c #f Type? RestDots?)
   tc-results/c
   . -> .
   Arrow?)
  (let* ([arg-len (length arg-list)]
         [tys-len (length arg-tys)]
         [arg-types
          (if (andmap type-annotation arg-list)
              (get-types arg-list #:default Univ)
              (cond
                [(= arg-len tys-len) arg-tys]
                [(< arg-len tys-len) (take arg-tys arg-len)]
                [(> arg-len tys-len)
                 (append arg-tys
                         (map (if (Type? rst)
                                  (λ _ rst)
                                  (λ _ -Bottom))
                              (drop arg-list tys-len)))]))])

    ;; Check that the number of formal arguments is valid for the expected type.
    ;; Thus it must be able to accept the number of arguments that the expected
    ;; type has. So we check for two cases: if the function doesn't accept
    ;; enough arguments, or if it requires too many arguments.
    ;; This allows a form like (lambda args body) to have the type (-> Symbol
    ;; Number) with out a rest arg.
    (when (or (and (< arg-len tys-len) (not rest-id))
              (and (> arg-len tys-len) (not rst)))
      (tc-error/delayed (expected-str tys-len rst arg-len rest-id)))
    (define rest-type
      (cond
        [(not rest-id) #f]
        [(RestDots? rst) rst]
        [(dotted? rest-id)
         => (λ (b) (make-RestDots (extend-tvars (list b) (get-type rest-id #:default Univ))
                                  b))]
        [else
         (define base-rest-type
           (cond
             [(type-annotation rest-id) (get-type rest-id #:default Univ)]
             [(Type? rst) rst]
             [(not rst) -Bottom]
             [else Univ]))
         (define extra-types
           (if (<= arg-len tys-len)
               (drop arg-tys arg-len)
               null))
         (apply Un base-rest-type extra-types)]))
    (tc-lambda-body arg-list arg-types
                    #:rest-arg+type (and rest-type (cons rest-id rest-type))
                    #:expected ret-ty
                    body)))

;; typecheck a single lambda, with argument list and body
;; drest-ty and drest-bound are both false or not false
;; tc/lambda-clause/check: formals syntax listof[Type?] tc-result
;;                         option[Type?] option[(cons Type? symbol)] -> arr?
(define (tc/lambda-clause/check formals body arg-tys ret-ty rst)
  (check-clause (formals-positional formals) (formals-rest formals) body arg-tys rst ret-ty))

;; typecheck a single opt-lambda clause with argument list and body
;; tc/opt-lambda-clause: listof[identifier] syntax -> listof[arr?]
(define (tc/opt-lambda-clause arg-list body aux-table flag-table)
  ;; arg-types: Listof[Type?]
  (define arg-types
    (for/list ([a (in-list arg-list)])
      (get-type a #:default (lambda ()
                              (define id (free-id-table-ref aux-table a #f))
                              (if id
                                  (get-type id #:default Univ)
                                  Univ)))))

  ;; new-arg-types: Listof[Listof[Type?]]
  (define new-arg-types
    (if (= 0 (free-id-table-count flag-table))
        (list arg-types)
        (apply append
               (for/list ([(k v) (in-free-id-table flag-table)])
                 (list
                  (for/list ([i (in-list arg-list)]
                             [t (in-list arg-types)])
                    (cond [(free-identifier=? i k) t]
                          [(free-identifier=? i v) (-val #t)]
                          [else t]))
                  (for/list ([i (in-list arg-list)]
                             [t (in-list arg-types)])
                    (cond [(free-identifier=? i k) (-val #f)]
                          [(free-identifier=? i v) (-val #f)]
                          [else t])))))))
  (for/list ([arg-types (in-list new-arg-types)])
    (tc-lambda-body arg-list arg-types body)))

;; restrict-to-arity : arr? nat -> (or/c #f arr?)
;; either produces a new arrow which is a subtype of arr with arity n,
;; or #f is that is not possible
(define (restrict-to-arity arr n)
  (match arr
    ;; currently does not handle rest arguments
    [(Arrow: args #f '() _)
     #:when (= n (length args))
     arr]
    [_ #f]))

;; formals syntax -> listof[arr?]
(define (tc/lambda-clause formals body)
  (define-values (aux-table flag-table)
    (syntax-parse body
      [(b:rebuild-let*) (values (attribute b.mapping) (attribute b.flag-mapping))]
      [_ (values (make-immutable-free-id-table)
                 (make-immutable-free-id-table))]))

  (define arg-list (formals-positional formals))
  (define rest-id (formals-rest formals))

  (define eta-expanded?
    (syntax-parse body
      [(((~literal #%plain-app) fun:id j:id ...)) ;; restricted to ids to avoid re-typechecking
       #:when (equal? (length arg-list)
                      (length (syntax->list #'(j ...))))
       #:when (andmap free-identifier=? arg-list (syntax->list #'(j ...)))
       #'fun]
      [_ #f]))
  
  (cond
    [(and (> (free-id-table-count aux-table) 0) (not rest-id))
     (tc/opt-lambda-clause arg-list body aux-table flag-table)]
    [else
     (define arg-types (get-types arg-list #:default (lambda () #f)))
     (define rest-type
       (cond
         ;; Lambda with poly dotted rest argument
         [(and rest-id (dotted? rest-id))
          =>
          (λ (bound)
            (unless (bound-index? bound)
              (if (bound-tvar? bound)
                  (tc-error "Bound on ... type (~a) is not an appropriate type variable" bound)
                  (tc-error/stx rest-id "Bound on ... type (~a) was not in scope" bound)))
            (make-RestDots (extend-tvars (list bound) (get-type rest-id #:default Univ))
                           bound))]
         ;; Lambda with regular rest argument
         [rest-id
          (get-type rest-id #:default Univ)]
         ;; Lambda with no rest argument
         [else #f]))
     (cond 
      ;; special case for un-annotated eta-expansions
      [(and eta-expanded? (not rest-id) (andmap not arg-types)
            ;; FIXME: should also handle polymorphic types
            ;; but we can't return anything but a (listof arr?) here 
            ;; FIXME: misses optimization opportunities in this code
            (match (tc-expr eta-expanded?)
              [(tc-result1: (Fun: arrs))
               (define possibles (for*/list ([arr (in-list arrs)]
                                             [restricted (in-value (restrict-to-arity arr (length arg-list)))]
                                             #:when restricted)
                                   restricted))
               (if (null? possibles)
                   #f
                   possibles)]
              [_ #f]))
       =>
       (lambda (x)
         (register-ignored! (car (syntax-e body)))
         x)]
      [else
       (list
        (tc-lambda-body arg-list (map (lambda (v) (or v Univ)) arg-types)
                        #:rest-arg+type (and rest-type (cons rest-id rest-type))
                        body))])]))


;; positional: (listof identifier?)
;; rest: id or #f
;; syntax: syntax? - the improper syntax list of identifiers
;; (i.e. (append positional (or id '())) but syntax)
(struct formals (positional rest syntax) #:transparent)

(define (make-formals stx)
  (let loop ([s stx] [acc null])
    (cond
      [(pair? s) (loop (cdr s) (cons (car s) acc))]
      [(null? s) (formals (reverse acc) #f stx)]
      [(pair? (syntax-e s)) (loop (stx-cdr s) (cons (stx-car s) acc))]
      [(null? (syntax-e s)) (formals (reverse acc) #f stx)]
      [else (formals (reverse acc) s stx)])))

;; Currently no support for objects representing the rest argument
(define (formals->objects formals)
  (for/list ([i (in-list (formals-positional formals))])
    (make-Path null i)))


;; An arity is a list (List Natural Boolean), with the number of positional
;; arguments and whether there is a rest argument.
;;
;; An arities-seen is a list (List (Listof Natural) (U Natural Infinity)),
;; with the list of positional only arities seen and the least number of
;; positional arguments on an arity with a rest argument seen.
(define (formals->arity formals)
  (list
    (length (formals-positional formals))
    (and (formals-rest formals) #t)))

(define initial-arities-seen (list empty +inf.0))

;; arities-seen-add : arities-seen? arity? -> arities-seen?
;; Adds the arity to the arities encoded in the arity-seen.
(define (arities-seen-add arities-seen arity)
  (match-define (list positionals min-rest) arities-seen)
  (match-define (list new-positional new-rest) arity)
  (define new-min-rest
    (if new-rest
        (min new-positional min-rest)
        min-rest))
  (list
    (filter (λ (n) (< n new-min-rest)) (cons new-positional positionals))
    new-min-rest))


;; arities-seen-seen-before? : arities-seen? arity? -> boolean?
;; Determines if the arity would have been covered by an existing arity in the arity-seen
(define (arities-seen-seen-before? arities-seen arity)
  (match-define (list positionals min-rest) arities-seen)
  (match-define (list new-positional new-rest) arity)
  (or (>= new-positional min-rest)
      (and (member new-positional positionals) (not new-rest))))


;; tc/mono-lambda : (listof (list formals syntax?)) (or/c #f tc-results) -> (listof arr?)
;; typecheck a sequence of case-lambda clauses
(define (tc/mono-lambda formals+bodies expected)
  (define expected-type
    (match expected
      [(tc-result1: t)
       (define resolved (resolve t))
       (match resolved
         [(Fun: arrows)
          #:when (for/and ([arr (in-list arrows)])
                   (null? (Arrow-kws arr)))
          resolved]
         [_ #f])]
      [_ #f]))

  ;; find-matching-arrs: (list/c natural? boolean?) arities-seen? -> (or #f Listof[arr?])
  ;; Returns a list when we know the expected type, and the list contains all the valid arrs that the
  ;; clause needs to type as.
  ;; Returns false if there is not enough information in the expected type to propogate down to the
  ;; clause
  (define (find-matching-arrs formal-arity arities-seen)
    (match-define (list formal-positionals formal-rest) formal-arity)
    (match expected-type
      [(Fun: arrows)
       #:when (for/and ([arr (in-list arrows)])
                (null? (Arrow-kws arr)))
       (for*/list ([a (in-list arrows)]
                   [dom (in-value (Arrow-dom a))]
                   [rst (in-value (Arrow-rst a))]
                   #:unless (arities-seen-seen-before? arities-seen (list (length dom) rst))
                   #:when (cond
                            [formal-rest
                             (or (Type? rst) (>= (length dom) formal-positionals))]
                            [else ((if rst <= =) (length dom) formal-positionals)]))
         a)]
      [_ #f]))


  ;; For each clause we figure out which arrs it needs to typecheck as,
  ;; and also which clauses are dead code.
  (define-values (used-formals+bodies+arrs arities-seen)
    (for/fold ([formals+bodies+arrs* empty]
               [arities-seen initial-arities-seen])
              ([formal+body (in-list formals+bodies)])
      (match formal+body
        [(list formal body)
         (define arity (formals->arity formal))
         (define matching-arrs (find-matching-arrs arity arities-seen))
         (values
           (cons
             (cond
               [(or (arities-seen-seen-before? arities-seen arity)
                    (null? matching-arrs))
                (warn-unreachable body)
                (add-dead-lambda-branch (formals-syntax formal))
                (list formal body null)]
               [else (list formal body matching-arrs)])
             formals+bodies+arrs*)
           (arities-seen-add arities-seen arity))])))

  (cond
    [(and (andmap (λ (f-b-arr) (empty? (third f-b-arr)))
                  used-formals+bodies+arrs)
          ;; If the empty function is expected, then don't error out
          (match expected-type
            [(Fun: (list)) #f]
            [_ #t]))
     ;; TODO improve error message.
     (tc-error/expr #:return (list (-Arrow null -Bottom #:rest Univ))
                    "Expected a function of type ~a, but got a function with the wrong arity"
                    expected-type)]
    [else
     (apply append
            (for/list ([fb* (in-list used-formals+bodies+arrs)])
              (match-define (list f* b* t*) fb*)
              (cond
                [(not t*) (tc/lambda-clause f* b*)]
                [else
                 (for/list ([arrow (in-list t*)])
                   (match arrow
                     [(Arrow: dom rst '() rng)
                      (tc/lambda-clause/check
                       f* b* dom (values->tc-results rng (formals->objects f*)) rst)]))])))]))

(define (tc/dep-lambda formalss-stx bodies-stx dep-fun-ty)
  (parameterize ([with-refinements? #t])
    (match-define (DepFun: raw-dom raw-pre raw-rng) dep-fun-ty)
    (define formalss (stx-map make-formals formalss-stx))
    (define bodies (syntax->list bodies-stx))
    (match* (formalss bodies)
      [((list fs) (list body))
       (cond
         [(not (= (length (formals-positional fs))
                  (length raw-dom)))
          (tc-error/expr #:return dep-fun-ty
                         (format "Expected ~a positional arguments, given ~a."
                                 (length raw-dom)
                                 (length (formals-positional fs))))]
         [(formals-rest fs)
          (tc-error/expr #:return dep-fun-ty
                         "Dependent functions do not currently support rest arguments.")]
         [else
          (define arg-names (formals-positional fs))
          (define dom (for/list ([d (in-list raw-dom)])
                        (instantiate-obj d arg-names)))
          (define pre (instantiate-obj raw-pre arg-names))
          (with-naively-extended-lexical-env
              [#:identifiers arg-names
               #:types dom
               #:props (list pre)]
            (tc-body/check body (values->tc-results raw-rng (map -id-path arg-names))))
          dep-fun-ty])]
      [(fs bs)
       (tc-error/expr #:return dep-fun-ty
                      "Dependent functions must have a single arity.")])))

(define (tc/mono-lambda/type formalss bodies expected)
  (match expected
    [(tc-result1:(? DepFun? dep-fun-ty))
     (tc/dep-lambda formalss bodies dep-fun-ty)]
    [_ (make-Fun
        (tc/mono-lambda
         (for/list ([f (in-syntax formalss)] [b (in-syntax bodies)])
           (list (make-formals f) b))
         expected))]))

(define (plambda-prop stx)
  (define d (plambda-property stx))
  (and d (car (flatten d))))

(define (has-poly-annotation? form)
  (or (plambda-prop form) (cons? (lookup-scoped-tvar-layer form))))

(define (remove-poly-layer tvarss)
  (filter cons? (map rest tvarss)))

(define (get-poly-layer tvarss)
  (map first tvarss))

(define (get-poly-tvarss form)
  (let ([plambda-tvars
          (let ([p (plambda-prop form)])
            (match (and p (map syntax-e (syntax->list p)))
              [#f #f]
              [(list var ... dvar '...)
               (list (list var dvar))]
              [(list id ...)
               (list id)]))]
        [scoped-tvarss
          (for/list ((tvarss (in-list (lookup-scoped-tvar-layer form))))
            (for/list ((tvar (in-list tvarss)))
              (match tvar
                [(list (list v ...) dotted-v)
                 (list (map syntax-e v) (syntax-e dotted-v))]
                [(list v ...) (map syntax-e v)])))])
    (if plambda-tvars
        (cons plambda-tvars scoped-tvarss)
        scoped-tvarss)))


;; tc/plambda syntax tvarss-list syntax-list syntax-list type -> Poly
;; formals and bodies must by syntax-lists
(define/cond-contract (tc/plambda form tvarss-list formals bodies expected)
  (syntax? (listof list?) syntax? syntax? (or/c tc-results/c #f) . -> . Type?)
  (define/cond-contract (maybe-loop form formals bodies expected)
    (syntax? syntax? syntax? (or/c tc-results/c #f) . -> . Type?)
    (match expected
      [(tc-result1: (app resolve (or (? Poly?) (? PolyDots?) (? PolyRow?))))
       (tc/plambda form (remove-poly-layer tvarss-list) formals bodies expected)]
      [_
        (define remaining-layers (remove-poly-layer tvarss-list))
        (if (null? remaining-layers)
            (tc/mono-lambda/type formals bodies expected)
            (tc/plambda form remaining-layers formals bodies expected))]))
  ;; check the bodies appropriately
  ;; and make both annotated and declared type variables point to the
  ;; same actual type variables (the fresh names)
  (define (extend-and-loop form ns formals bodies expected)
    (let loop ((tvarss tvarss))
      (match tvarss
        [(list) (maybe-loop form formals bodies expected)]
        [(cons (list (list tvars ...) dotted) rest-tvarss)
         (extend-indexes dotted
            (extend-tvars/new tvars ns
              (loop rest-tvarss)))]
        [(cons tvars rest-tvarss)
         (extend-tvars/new tvars ns
           (loop rest-tvarss))])))
  (define tvarss (get-poly-layer tvarss-list))

  (match expected
    [(tc-result1: (app resolve (and t (Poly-fresh: ns fresh-ns expected*))))
     ;; make sure the declared and annotated type variable arities match up
     ;; with the expected type variable arity
     (for ((tvars (in-list tvarss)))
       (when (and (cons? tvars) (list? (first tvars)))
         (tc-error
          "Expected a polymorphic function without ..., but given function/annotation had ..."))
       (unless (= (length tvars) (length fresh-ns))
         (tc-error "Expected ~a type variables, but given ~a"
                   (length fresh-ns) (length tvars))))
     (make-Poly #:original-names ns fresh-ns (extend-and-loop form fresh-ns formals bodies (ret expected*)))]
    [(tc-result1: (app resolve (and t (PolyDots-names: (list ns ... dvar) expected*))))
     ;; make sure the declared and annotated type variable arities match up
     ;; with the expected type variable arity
     (for ((tvars (in-list tvarss)))
       (match tvars
         [(list (list vars ...) dotted)
          (unless (= (length vars) (length ns))
            (tc-error "Expected ~a non-dotted type variables, but given ~a"
                      (length ns) (length vars)))]
         [else
           (tc-error "Expected a polymorphic function with ..., but function/annotation had no ...")]))
     (make-PolyDots (append ns (list dvar)) (extend-and-loop form ns formals bodies (ret expected*)))]
    [(tc-result1: (app resolve (and t (PolyRow-fresh: ns fresh-ns constraints expected*))))
     (for ((tvars (in-list tvarss)))
       (when (and (cons? tvars) (list? (first tvars)))
         (tc-error
          "Expected a polymorphic function without ..., but given function/annotation had ..."))
       (unless (= (length tvars) 1)
         (tc-error "Expected ~a type variable, but given ~a"
                   1 (length tvars))))
     (make-PolyRow
      #:original-names ns
      fresh-ns
      constraints
      (extend-and-loop form fresh-ns
                       formals bodies (ret expected*)))]
    [_
     (define lengths
       (for/set ((tvars (in-list tvarss)))
         (match tvars
           [(list (list vars ...) dotted)
            (length vars)]
           [(list vars ...)
            (length vars)])))
     (define dots
       (for/set ((tvars (in-list tvarss)))
         (match tvars
           [(list (list vars ...) dotted) #t]
           [(list vars ...) #f])))
     (unless (= 1 (set-count lengths))
      (tc-error "Expected annotations to have the same number of type variables, but given ~a"
                (set->list lengths)))
     (unless (= 1 (set-count dots))
      (tc-error "Expected annotations to all have ... or none to have ..., but given both"))
     (define dotted (and (set-first dots) (second (first tvarss))))
     (define ns (build-list (set-first lengths) (lambda (_) (gensym))))
     (define results (extend-and-loop form ns formals bodies expected))
     (if dotted
         (make-PolyDots (append ns (list dotted)) results)
         (make-Poly #:original-names (first tvarss) ns results))]))

;; typecheck a sequence of case-lambda clauses, which is possibly polymorphic
;; tc/lambda : syntax syntax-list syntax-list (or/c tc-results #f) -> tc-results
(define (tc/lambda form formals bodies expected)
  (if (or (has-poly-annotation? form)
          (match expected
            [(tc-result1: (app resolve t)) (or (Poly? t) (PolyDots? t) (PolyRow? t))]
            [_ #f]))
      (ret (tc/plambda form (get-poly-tvarss form) formals bodies expected) -true-propset)
      (ret (tc/mono-lambda/type formals bodies expected) -true-propset)))

;; formals : the formal arguments to the loop
;; body : a block containing the body of the loop
;; name : the name of the loop
;; args : the types of the actual arguments to the loop
;; ret : the expected return type of the whole expression
;; Returns both the tc-results of the function and of the body
(define (tc/rec-lambda/check formals* body name args return)
  (define formals (syntax->list formals*))
  (define ft (t:->* args (tc-results->values return)))
  (define names (cons name formals))
  (with-extended-lexical-env
    [#:identifiers (cons name formals)
     #:types (cons ft args)]
    (values
     (erase-identifiers (ret ft) names)
     (erase-identifiers (tc-body/check body return) names))))
