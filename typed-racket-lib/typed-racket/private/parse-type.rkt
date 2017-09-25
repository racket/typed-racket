#lang racket/base

;; This module provides functions for parsing types written by the user

(require (rename-in "../utils/utils.rkt" [infer infer-in])
         (rep core-rep type-rep object-rep values-rep free-ids rep-utils)
         (types abbrev utils prop-ops resolve
                classes prefab signatures
                subtype path-type numeric-tower)
         (only-in (infer-in infer) intersect)
         (utils tc-utils stxclass-util literal-syntax-class)
         syntax/stx (prefix-in c: (contract-req))
         syntax/parse racket/sequence
         (env tvar-env type-alias-env mvar-env
              lexical-env index-env row-constraint-env
              signature-env)
         racket/dict
         racket/list
         racket/promise
         racket/format
         racket/match
         "parse-classes.rkt"
         (for-template "../base-env/base-types-extra.rkt"
                       racket/base)
         (for-label
           (except-in racket/base case-lambda)
           racket/unit
           "../base-env/colon.rkt"
           "../base-env/base-types-extra.rkt"
           ;; match on the `case-lambda` binding in the TR primitives
           ;; rather than the one from Racket, which is no longer bound
           ;; in most TR modules.
           (only-in "../base-env/case-lambda.rkt" case-lambda)))

(provide/cond-contract ;; Parse the given syntax as a type
                       [parse-type (syntax? . c:-> . Type?)]
                       ;; Parse the given identifier using the lexical
                       ;; context of the given syntax object
                       [parse-type/id (syntax? c:any/c . c:-> . Type?)]
                       [parse-tc-results (syntax? . c:-> . tc-results/c)]
                       [parse-literal-alls (syntax? . c:-> . (c:listof (c:or/c (c:listof identifier?) (c:list/c (c:listof identifier?) identifier?))))]
                       ;; Parse a row, which is only allowed in row-inst
                       [parse-row (syntax? . c:-> . Row?)])

(provide star ddd/bound
         current-referenced-aliases
         current-referenced-class-parents
         current-type-alias-name)

;; current-term-names : Parameter<(Listof Id)>
;; names currently "bound" by a type we are parsing
;; e.g. (Refine [x : τ] ψ) -- x would be added to
;; current-term-names when parsing ψ
(define current-local-term-ids (make-parameter '()))

(define-syntax-rule (with-local-term-names bindings e ...)
  (parameterize ([current-local-term-ids
                  (append bindings (current-local-term-ids))])
    e ...))

(define (local-term-id x)
  (cond
    [(assoc x (current-local-term-ids) free-identifier=?)
     => cdr]
    [else #f]))

;; current-type-alias-name : Parameter<(Option Id)>
;; This parameter stores the name of the type alias that is
;; being parsed (set in type-alias-helper.rkt), #f if the
;; parsing is not for a type alias
(define current-type-alias-name (make-parameter #f))

;; current-referenced-aliases : Parameter<Option<Box<List<Id>>>>
;; This parameter is used to coordinate with the type-checker to determine
;; if a type alias should be recursive or not
;;
;; interp. the argument is #f if not checking for type aliases.
;;         Otherwise, it should be a box containing a list of
;;         identifiers (i.e., type aliases in the syntax)
(define current-referenced-aliases (make-parameter #f))

;; current-referenced-class-parents : Parameter<Option<Box<List<Id>>>>
;; This parameter is used to coordinate with the type-checker about
;; the dependency structure of class types using #:implements
;;
;; interp. same as above
(define current-referenced-class-parents (make-parameter #f))

;; current-arities : Parameter<(Listof Natural)>
;; Represents the stack of function arities in the potentially
;; nested function type being parsed. The stack does not include the
;; innermost function (since syntax classes can check that locally)
(define current-arities (make-parameter null))

(define-syntax-rule (with-arity arity e ...)
  (parameterize ([current-arities (cons arity (current-arities))])
    e ...))

(define-literal-syntax-class #:for-label car)
(define-literal-syntax-class #:for-label cdr)
(define-literal-syntax-class #:for-label vector-length)
(define-literal-syntax-class #:for-label colon^ (:))
(define-literal-syntax-class #:for-label quote)
(define-literal-syntax-class #:for-label cons)
(define-literal-syntax-class #:for-label Class)
(define-literal-syntax-class #:for-label Object)
(define-literal-syntax-class #:for-label Row)
(define-literal-syntax-class #:for-label Unit)
(define-literal-syntax-class #:for-label import)
(define-literal-syntax-class #:for-label export)
(define-literal-syntax-class #:for-label init-depend)
(define-literal-syntax-class #:for-label Refinement)
(define-literal-syntax-class #:for-label Instance)
(define-literal-syntax-class #:for-label List)
(define-literal-syntax-class #:for-label List*)
(define-literal-syntax-class #:for-label pred)
(define-literal-syntax-class #:for-label ->)
(define-literal-syntax-class #:for-label ->*)
(define-literal-syntax-class #:for-label case->^ (case-> case-lambda))
(define-literal-syntax-class #:for-label Rec)
(define-literal-syntax-class #:for-label U)
(define-literal-syntax-class #:for-label Union)
(define-literal-syntax-class #:for-label All)
(define-literal-syntax-class #:for-label Opaque)
(define-literal-syntax-class #:for-label Parameter)
(define-literal-syntax-class #:for-label Vector)
(define-literal-syntax-class #:for-label Struct)
(define-literal-syntax-class #:for-label Struct-Type)
(define-literal-syntax-class #:for-label Prefab)
(define-literal-syntax-class #:for-label Values)
(define-literal-syntax-class #:for-label values)
(define-literal-syntax-class #:for-label AnyValues)
(define-literal-syntax-class #:for-label Top)
(define-literal-syntax-class #:for-label Bot)
(define-literal-syntax-class #:for-label Distinction)
(define-literal-syntax-class #:for-label Sequenceof)
(define-literal-syntax-class #:for-label ∩)
(define-literal-syntax-class #:for-label Intersection)
(define-literal-syntax-class #:for-label Refine)
(define-literal-syntax-class #:for-label not)
(define-literal-syntax-class #:for-label and)
(define-literal-syntax-class #:for-label or)
(define-literal-syntax-class #:for-label unless)
(define-literal-syntax-class #:for-label when)
(define-literal-syntax-class #:for-label if)
(define-literal-syntax-class #:for-label <)
(define-literal-syntax-class #:for-label <=)
(define-literal-syntax-class #:for-label >)
(define-literal-syntax-class #:for-label >=)
(define-literal-syntax-class #:for-label =)
(define-literal-syntax-class #:for-label *)
(define-literal-syntax-class #:for-label +)
(define-literal-syntax-class #:for-label -)



;; (Syntax -> Type) -> Syntax Any -> Syntax
;; See `parse-type/id`. This is a curried generalization.
(define ((parse/id p) loc datum)
  (let* ([stx* (datum->syntax loc datum loc loc)])
    (p stx*)))

(define (parse-literal-alls stx)
  (syntax-parse stx
    [(:All^ (~or (vars:id ... v:id dd:ddd) (vars:id ...)) . t:omit-parens)
     (define vars-list (syntax->list #'(vars ...)))
     (cons (if (attribute v)
               (list vars-list #'v)
               vars-list)
           (parse-literal-alls #'t.type))]
    [_ null]))


;; Syntax -> Type
;; Parse a Forall type
(define (parse-all-type stx)
  (syntax-parse stx
    [(:All^ (vars:id ... v:id dd:ddd) . t:omit-parens)
     (define maybe-dup (check-duplicate-identifier (syntax->list #'(vars ... v))))
     (when maybe-dup
       (parse-error "duplicate type variable or index"
                    "variable or index" (syntax-e maybe-dup)))
     (let* ([vars (stx-map syntax-e #'(vars ...))]
            [v (syntax-e #'v)])
       (extend-indexes v
         (extend-tvars vars
           (make-PolyDots (append vars (list v)) (parse-type #'t.type)))))]
    [(:All^ (vars:id ...) . t:omit-parens)
     (define maybe-dup (check-duplicate-identifier (syntax->list #'(vars ...))))
     (when maybe-dup
       (parse-error "duplicate type variable"
                    "variable" (syntax-e maybe-dup)))
     (let* ([vars (stx-map syntax-e #'(vars ...))])
       (extend-tvars vars
         (make-Poly vars (parse-type #'t.type))))]
    ;; Next two are row polymorphic cases
    [(:All^ (var:id #:row) . t:omit-parens)
     (add-disappeared-use #'kw)
     (define var* (syntax-e #'var))
     ;; When we're inferring the row constraints, there
     ;; should be no need to extend the constraint environment
     (define body-type
       (extend-tvars (list var*) (parse-type #'t.type)))
     (make-PolyRow
      (list var*)
      ;; No constraints listed, so we need to infer the constraints
      (infer-row-constraints body-type)
      body-type)]
    [(:All^ (var:id #:row constr:row-constraints) . t:omit-parens)
     (add-disappeared-use #'kw)
     (define var* (syntax-e #'var))
     (define constraints (attribute constr.constraints))
     (extend-row-constraints (list var*) (list constraints)
       (extend-tvars (list var*)
         (make-PolyRow
          (list var*)
          constraints
          (parse-type #'t.type))))]
    [(:All^ (_:id ...) _ _ _ ...) (parse-error "too many forms in body of All type")]
    [(:All^ . rest) (parse-error "bad syntax")]))

;; syntax class for standard keyword syntax (same as contracts), may be
;; optional or mandatory depending on where it's used
(define-splicing-syntax-class plain-kw-tys
  (pattern (~seq k:keyword t:expr)
           #:attr mand-kw (delay (make-Keyword (syntax-e #'k) (parse-type #'t) #t))
           #:attr opt-kw  (delay (make-Keyword (syntax-e #'k) (parse-type #'t) #f))))

(define-splicing-syntax-class keyword-tys
  (pattern kw:plain-kw-tys #:attr Keyword (attribute kw.mand-kw))
  ;; custom optional keyword syntax for TR
  (pattern (~seq [k:keyword t:expr])
           #:attr Keyword (delay (make-Keyword (syntax-e #'k) (parse-type #'t) #f))))

(define-syntax-class non-keyword-ty
  (pattern (k:expr e ...))
  (pattern (~and t:expr (~not :colon^) (~not :->^))
           #:when (not (syntax->list #'t))))

;; TR type ASTs only allow identifiers either that
;; a) are identifier-binding #t
;; b) are generated by TR for internal use
;;    (i.e. see genid and related functions in utils/utils.rkt)
;; This function helps us create a fresh identifier
;; that looks the same as the original and which meets the above
;; conditions.
(define (id->local-id id)
  (gen-pretty-id (syntax->datum id)))

(define-syntax-class dependent-fun-arg
  #:description "dependent function argument"
  #:attributes (name local-name deps type-stx)
  (pattern [name:id
            :colon^
            (~optional (dep:id ...)
                       #:defaults ([(dep 1) null]))
            type-stx:expr]
           #:attr local-name (id->local-id #'name)
           #:attr deps (syntax->list #'(name dep ...))))

;; syntax classes for parsing ->* function types
(define-syntax-class (->*-args mand?)
  #:description "type arguments for ->*"
  #:attributes (doms kws)
  (pattern ((~var dom (->*-dom mand?)) ...)
           #:do [(define-values (kws doms)
                   ;; would like to use `partition` but we need to traverse multiple
                   ;; lists rather than just checking Keyword? due to laziness
                   (for/fold ([kws null] [doms null])
                             ([kw? (in-list (attribute dom.kw?))]
                              [type/kw (attribute dom.result)])
                     (if kw?
                         (values (cons type/kw kws) doms)
                         (values kws (cons type/kw doms)))))]
           #:attr doms (reverse doms)
           #:attr kws (reverse kws)))

;; parameterized syntax class for parsing ->* domains.
(define-splicing-syntax-class (->*-dom mand?)
  #:attributes (kw? result)
  (pattern (~seq k:keyword t:expr)
           #:attr kw? #t
           #:attr result
           (delay (make-Keyword (syntax-e #'k) (parse-type #'t) mand?)))
  (pattern t:expr
           #:attr kw? #f
           ;; does not need to be delayed since there's no parsing done
           #:attr result #'t))

(define-splicing-syntax-class ->*-rest
  #:description "rest argument type for ->*"
  #:attributes (type)
  (pattern (~optional (~seq #:rest type:non-keyword-ty))))

;; syntax classes for props, objects, and related things
(define-syntax-class legacy-path-elem
  #:description "path element"
  (pattern :car^
           #:attr val -car)
  (pattern :cdr^
           #:attr val -cdr))


(define-syntax-class @
  #:description "@"
  (pattern (~datum @)))

(define-syntax-class !
  #:description "!"
  (pattern (~datum !)))

(define-splicing-syntax-class legacy-simple-latent-prop
  #:description "latent prop"
  (pattern (~seq t:expr :@ pe:legacy-path-elem ...)
           #:attr type (parse-type #'t)
           #:attr path (attribute pe.val))
  (pattern t:expr
           #:attr type (parse-type #'t)
           #:attr path '()))

(define (parse-prop stx)
  (syntax-parse stx
    [p:proposition (attribute p.val)]))

(define int-comps (list (cons '<= -leq)
                        (cons '< -lt)
                        (cons '>= -geq)
                        (cons '> -gt)
                        (cons '= -eq)))

(define-syntax-class proposition
  #:description "proposition"
  #:attributes (val)
  (pattern :Top^ #:attr val -tt)
  (pattern :Bot^ #:attr val -ff)
  (pattern (:colon^ o:symbolic-object t:expr)
           #:attr val (-is-type (attribute o.val) (parse-type #'t)))
  (pattern (:! o:symbolic-object t:expr)
           #:attr val (-not-type (attribute o.val) (parse-type #'t)))
  (pattern (:and^ p:proposition ...)
           #:attr val (apply -and (attribute p.val)))
  (pattern (:or^ p:proposition ...)
           #:attr val (apply -or (attribute p.val)))
  (pattern (:unless^ p1:proposition p2:proposition)
           #:attr val (-or (attribute p1.val) (attribute p2.val)))
  (pattern (:when^ p1:proposition p2:proposition)
           #:attr val (-or (negate-prop (attribute p1.val))
                           (-and (attribute p1.val) (attribute p2.val))))
  (pattern (:if^ p1:proposition p2:proposition p3:proposition)
           #:attr val (let ([tst (attribute p1.val)]
                            [thn (attribute p2.val)]
                            [els (attribute p3.val)])
                        (-or (-and tst thn)
                             (-and (negate-prop tst) els))))
  (pattern (:not^ p:proposition)
           #:attr val (negate-prop (attribute p.val)))
  (pattern ((~and comp (~or :<=^ :<^ :>=^ :>^ :=^))
            obj0:inequality-symbolic-object
            obj1:inequality-symbolic-object
            objs:inequality-symbolic-object
            ...)
           #:attr val
           (let ([mk (cdr (assq (syntax-e #'comp) int-comps))])
             (let loop ([ps (list (mk (attribute obj0.val)
                                      (attribute obj1.val)))]
                        [lhs (attribute obj1.val)]
                        [remaining (attribute objs.val)])
               (match remaining
                 [(cons rhs os)
                  (loop (cons (mk lhs rhs) ps)
                        rhs
                        os)]
                 [_ (apply -and ps)])))))


(define-syntax-class inequality-symbolic-object
  #:description "symbolic object in an inequality"
  #:attributes (val)
  (pattern o:symbolic-object
           #:do [(define obj (attribute o.val))
                 (define obj-ty (lookup-obj-type/lexical obj))]
           #:fail-unless (subtype obj-ty -Int)
           (format "terms in linear constraints must be integers, got ~a for ~a"
                   obj-ty obj)
           #:attr val (attribute o.val)))

(define (parse-obj stx)
  (syntax-parse stx
    [o:symbolic-object (attribute o.val)]))

(define-syntax-class symbolic-object
  #:description "symbolic object"
  #:attributes (val)
  (pattern (:+^ . body)
           #:attr val (parse-linear-expression-body #'body #t))
  (pattern (:-^ . body)
           #:attr val (parse-linear-expression-body #'body #f))
  (pattern (:*^ ~! n:exact-integer o:symbolic-object-w/o-lexp)
           #:do [(define obj (attribute o.val))
                 (define obj-ty (lookup-obj-type/lexical obj))]
           #:fail-unless (subtype obj-ty -Int)
           (format "terms in linear constraints must be integers, got ~a for ~a"
                   obj-ty obj)
           #:attr val (-lexp (list (syntax->datum #'n) (attribute o.val))))
  (pattern n:exact-integer
           #:attr val (-lexp (syntax->datum #'n)))
  (pattern o:symbolic-object-w/o-lexp
           #:attr val (attribute o.val))
  )


(define-syntax-class symbolic-object-w/o-lexp
  #:description "symbolic object"
  #:attributes (val)
  (pattern i:id
           #:fail-unless (or (identifier-binding #'i)
                             (local-term-id #'i))
           "Propositions may not reference identifiers that are unbound"
           #:fail-when (is-var-mutated? #'i)
           "Propositions may not reference identifiers that are mutated"
           #:attr val (match (local-term-id #'i)
                        [#f (lookup-alias/lexical #'i)]
                        [local-id (lookup-alias/lexical local-id)]))
  (pattern (:car^ ~! o:symbolic-object-w/o-lexp)
           #:do [(define obj (attribute o.val))
                 (define obj-ty (lookup-obj-type/lexical obj))]
           #:fail-unless (subtype obj-ty (-pair Univ Univ))
           (format "car expects a pair, but got ~a for ~a"
                   obj-ty obj)
           #:attr val (-car-of (attribute o.val)))
  (pattern (:cdr^ ~! o:symbolic-object-w/o-lexp)
           #:do [(define obj (attribute o.val))
                 (define obj-ty (lookup-obj-type/lexical obj))]
           #:fail-unless (subtype obj-ty (-pair Univ Univ))
           (format "cdr expects a pair, but got ~a for ~a"
                   obj-ty obj)
           #:attr val (-cdr-of (attribute o.val)))
  (pattern (:vector-length^ ~! o:symbolic-object-w/o-lexp)
           #:do [(define obj (attribute o.val))
                 (define obj-ty (lookup-obj-type/lexical obj))]
           #:fail-unless (subtype obj-ty -VectorTop)
           (format "vector-length expects a vector, but got ~a for ~a"
                   obj-ty obj)
           #:attr val (-vec-len-of (attribute o.val))))

(define (parse-linear-expression-body body plus?)
  (syntax-parse body
    [(t:linear-expression-term ts:linear-expression-term ...)
     (cond
       [plus?
        (apply -lexp (attribute t.val) (attribute ts.val))]
       [else
        (apply -lexp
               (attribute t.val)
               (for/list ([term (in-list (attribute ts.val))])
                 (scale-obj -1 term)))])]))

(define-syntax-class linear-expression-term
  #:description "symbolic object"
  #:attributes (val)
  (pattern (:*^ ~! coeff:exact-integer o:symbolic-object-w/o-lexp)
           #:do [(define obj (attribute o.val))
                 (define obj-ty (lookup-obj-type/lexical obj))]
           #:fail-unless (subtype obj-ty -Int)
           (format "terms in linear expressions must be integers, got ~a for ~a"
                   obj-ty obj)
           #:attr val (-lexp (list (syntax->datum #'coeff) (attribute o.val))))
  (pattern n:exact-integer
           #:attr val (-lexp (syntax-e (attribute n))))
  (pattern o:symbolic-object-w/o-lexp
           #:do [(define obj (attribute o.val))
                 (define obj-ty (lookup-obj-type/lexical obj))]
           #:fail-unless (subtype obj-ty -Int)
           (format "terms in linear expressions must be integers, got ~a for ~a"
                   obj-ty obj)
           #:attr val (attribute o.val))
  )

;; old + deprecated
(define-syntax-class (legacy-prop doms)
  #:description "proposition"
  #:attributes (prop)
  (pattern :Top^ #:attr prop -tt)
  (pattern :Bot^ #:attr prop -ff)
  ;; Here is wrong check
  (pattern (t:expr :@ ~! pe:legacy-path-elem ... (~var o (legacy-prop-obj doms)))
           #:attr prop (-is-type (-acc-path (attribute pe.val) (attribute o.obj)) (parse-type #'t)))
  ;; Here is wrong check
  (pattern (:! t:expr :@ ~! pe:legacy-path-elem ... (~var o (legacy-prop-obj doms)))
           #:attr prop (-not-type (-acc-path (attribute pe.val) (attribute o.obj)) (parse-type #'t)))
  (pattern (:! t:expr)
           #:attr prop (-not-type 0 (parse-type #'t)))
  (pattern ((~datum and) (~var p (legacy-prop doms)) ...)
           #:attr prop (apply -and (attribute p.prop)))
  (pattern ((~datum or) (~var p (legacy-prop doms)) ...)
           #:attr prop (apply -or (attribute p.prop)))
  (pattern ((~literal implies) (~var p1 (legacy-prop doms)) (~var p2 (legacy-prop doms)))
           #:attr prop (-or (negate-prop (attribute p1.prop)) (attribute p2.prop)))
  (pattern t:expr
           #:attr prop (-is-type 0 (parse-type #'t))))

(define-splicing-syntax-class (legacy-prop-obj doms)
  #:description "prop object"
  #:attributes (obj)
  (pattern i:id
           #:fail-unless (identifier-binding #'i)
           "Propositions for predicates may not reference identifiers that are unbound"
           #:fail-when (is-var-mutated? #'i)
           "Propositions for predicates may not reference identifiers that are mutated"
           #:attr obj (-id-path #'i))
  (pattern idx:nat
           #:do [(define arg (syntax-e #'idx))]
           #:fail-unless (< arg (length doms))
           (format "Proposition's object index ~a is larger than argument length ~a"
                   arg (length doms))
           #:attr obj (-arg-path arg 0))
  (pattern (~seq depth-idx:nat idx:nat)
           #:do [(define arg (syntax-e #'idx))
                 (define depth (syntax-e #'depth-idx))]
           #:fail-unless (<= depth (length (current-arities)))
           (format "Index ~a used in a proposition, but the use is only within ~a enclosing functions"
                   depth (length (current-arities)))
           #:do [(define actual-arg
                   (if (zero? depth)
                       (length doms)
                       (list-ref (current-arities) (sub1 depth))))]
           #:fail-unless (< arg actual-arg)
           (format "Proposition's object index ~a is larger than argument length ~a"
                   depth actual-arg)
           #:attr obj (-arg-path arg (syntax-e #'depth-idx))))


(define-syntax-class legacy-object
  #:attributes (object)
  (pattern e:expr
           #:attr object -empty-obj))

(define-splicing-syntax-class (legacy-full-latent doms)
  #:description "latent propositions and object"
  (pattern (~seq (~optional (~seq #:+ (~var p+ (legacy-prop doms)) ...+) #:defaults ([(p+.prop 1) null]))
                 (~optional (~seq #:- (~var p- (legacy-prop doms)) ...+) #:defaults ([(p-.prop 1) null]))
                 (~optional (~seq #:object o:legacy-object)))
           #:attr positive (apply -and (attribute p+.prop))
           #:attr negative (apply -and (attribute p-.prop))
           #:attr object (or (attribute o.object) -empty-obj)))

(define (parse-types stx-list)
  (stx-map parse-type stx-list))

(define (parse-quoted-type stx)
  (syntax-parse stx
    [(t1 . t2)
     (-pair (parse-quoted-type #'t1) (parse-quoted-type #'t2))]
    [t
     (-val (syntax->datum #'t))]))

(define (parse-type stx)
  (parameterize ([current-orig-stx stx])
    (syntax-parse
        stx
      [t
       #:declare t (3d Type?)
       (attribute t.datum)]
      [(fst . rst)
       #:fail-unless (not (syntax->list #'rst)) #f
       (-pair (parse-type #'fst) (parse-type #'rst))]
      [(:Class^ e ...)
       (parse-class-type stx)]
      [(:Object^ e ...)
       (parse-object-type stx)]
      [(:Refinement^ p?:id)
       (match (lookup-id-type/lexical #'p?)
         [(and t (Fun: (list (Arrow: (list dom) #f '() _))))
          (make-Refinement dom #'p?)]
         [t (parse-error "expected a predicate for argument to Refinement"
                         "given" t)])]
      [(:Struct^ t)
       (let ([v (parse-type #'t)])
         (match (resolve v)
           [(and s (? Struct?)) (make-StructTop s)]
           [_ (parse-error #:delayed? #t
                           "expected a structure type for argument to Struct"
                           "given" v)
              (Un)]))]
      [(:Struct-Type^ t)
       (define v (parse-type #'t))
       (match (resolve v)
         [(or (? Struct? s) (? Prefab? s)) (make-StructType s)]
         [_ (parse-error #:delayed? #t
                         "expected a structure type for argument to Struct-Type"
                         "given" v)
            (Un)])]
      [(:Prefab^ key ts ...)
       #:fail-unless (prefab-key? (syntax->datum #'key)) "expected a prefab key"
       (define num-fields (length (syntax->list #'(ts ...))))
       (define new-key (normalize-prefab-key (syntax->datum #'key) num-fields))
       (unless (= (prefab-key->field-count new-key) num-fields)
         (parse-error "the number of fields in the prefab key and type disagree"
                      "key" (prefab-key->field-count new-key)
                      "fields" num-fields))
       (make-Prefab new-key (parse-types #'(ts ...)))]
      [(:Refine^ [x:id :colon^ type:expr] prop:expr)
       ;; x is not in scope for the type
       (define t (parse-type #'type))
       ;; create a fresh local name valid for TR type ASTs
       (define x-local (id->local-id #'x))
       ;; extend lexical type env with 'x-local' ∈ 't'
       ;; while we parse 'prop', also record that in this scope,
       ;; 'x' maps to 'x-local'
       (define p
         (with-extended-lexical-env
             [#:identifiers (list x-local)
              #:types (list t)]
           (with-local-term-names (list (cons #'x x-local))
             (parse-prop #'prop))))
       ;; build the refinement type!
       (define refinement-type (-refine x-local t p))
       ;; record the name for printing purposes
       (save-term-var-names! refinement-type (list x-local))
       ;; and return the refinement type
       refinement-type]
      [(:Instance^ t)
       (let ([v (parse-type #'t)])
         (if (not (or (F? v) (Mu? v) (Name? v) (Class? v) (Error? v)))
             (begin (parse-error #:delayed? #t
                                 "expected a class type for argument to Instance"
                                 "given" v)
                    (make-Instance (Un)))
             (make-Instance v)))]
      [(:Unit^ (:import^ import:id ...)
               (:export^ export:id ...)
               (~optional (:init-depend^ init-depend:id ...) 
                          #:defaults ([(init-depend 1) null]))
               (~optional result
                          #:defaults ([result #f])))
       ;; Lookup an identifier in the signature environment
       ;; Fail with a parse error, if the lookup returns #f
       (define (id->sig id)
         (or (lookup-signature id)
             (parse-error #:stx id
                          #:delayed? #f
                          "Unknown signature used in Unit type"
                          "signature" (syntax-e id))))
       (define (import/export-error)
         (parse-error #:stx stx
                      #:delayed? #f
                      "Unit types must import and export distinct signatures"))
       (define (init-depend-error)
         (parse-error
          #:stx stx
          #:delayed? #f
          "Unit type initialization dependencies must be a subset of imports"))
       (define imports
         (check-imports/exports (stx-map id->sig #'(import ...)) import/export-error))
       (define exports
         (check-imports/exports (stx-map id->sig #'(export ...)) import/export-error))
       (define init-depends
         (check-init-depends/imports (stx-map id->sig #'(init-depend ...))
                                     imports
                                     init-depend-error))
       (define res (attribute result))
       (make-Unit imports
                  exports
                  init-depends
                  (if res (parse-values-type res) (-values (list -Void))))]
      [(:List^ ts ...)
       (parse-list-type stx)]
      [(:List*^ ts ... t)
       (-Tuple* (parse-types #'(ts ...)) (parse-type #'t))]
      [(:Vector^ ts ...)
       (make-HeterogeneousVector (parse-types #'(ts ...)))]
      [(:cons^ fst rst)
       (-pair (parse-type #'fst) (parse-type #'rst))]
      [(:pred^ t)
       (make-pred-ty (parse-type #'t))]
      [(:case->^ tys ...)
       (make-Fun
        (for/list ([ty (in-syntax #'(tys ...))])
          (let ([t (parse-type ty)])
            (match t
              [(Fun: (list arr)) arr]
              [_ (parse-error
                  #:stx ty
                  "expected a function type for component of case-> type"
                  "given" t)]))))]
      [(:Rec^ x:id t)
       (let* ([var (syntax-e #'x)]
              [tvar (make-F var)])
         (extend-tvars (list var)
                       (let ([t* (parse-type #'t)])
                         ;; is t in a productive position?
                         (define productive
                           (let/ec return
                             (let check ([ty t*])
                               (match ty
                                 [(== tvar) (return #f)]
                                 [(Union: _ elems) (for-each check elems)]
                                 [(Intersection: elems prop)
                                  (for-each check elems)
                                  (check prop)]
                                 [(App: rator rands)
                                  (check (resolve-app rator rands stx))]
                                 [(Mu: _ body) (check body)]
                                 [(Poly: names body) (check body)]
                                 [(PolyDots: names body) (check body)]
                                 [(PolyRow: _ _ body) (check body)]
                                 [(? Type?) (void)]
                                 [(? Rep?) (Rep-for-each check ty)]
                                 [_ (void)]))
                             #t))
                         (unless productive
                           (parse-error
                            #:stx stx
                            "recursive types are not allowed directly inside their definition"))
                         (if (memq var (fv t*))
                             (make-Mu var t*)
                             t*))))]
      [((~or :U^ :Union^) ts ...)
       (apply Un (parse-types #'(ts ...)))]
      [((~or :∩^ :Intersection^) ts ...)
       (for/fold ([ty Univ])
                 ([t (in-list (parse-types #'(ts ...)))])
         (intersect ty t))]
      [(:quote^ t)
       (parse-quoted-type #'t)]
      [(:All^ . rest)
       (parse-all-type stx)]
      [(:Opaque^ p?:id)
       (make-Opaque #'p?)]
      [(:Distinction^ name:id unique-id:id rep-ty:expr)
       (-Distinction (syntax-e #'name) (syntax-e #'unique-id) (parse-type #'rep-ty))]
      [(:Parameter^ t)
       (let ([ty (parse-type #'t)])
         (-Param ty ty))]
      [(:Parameter^ t1 t2)
       (-Param (parse-type #'t1) (parse-type #'t2))]
      [((~and p :Parameter^) args ...)
       (parse-error
        #:stx stx
        (~a (syntax-e #'p) " expects one or two type arguments, given "
            (sub1 (length (syntax->list #'(args ...))))))]
      [(:Sequenceof^ t ...)
       (apply -seq (parse-types #'(t ...)))]
      ;; simple dependent functions
      ;; e.g. (-> ([x : τ] ...+) τ)
      [(:->^ (args:dependent-fun-arg ...+)
             ~!
             (~optional (~seq #:pre (pre-dep-stx:id ...) pre-stx:expr)
                        #:defaults ([pre-stx #'Top]
                                    [(pre-dep-stx 1) null]))
             rng-type:non-keyword-ty
             (~seq (~optional (~seq #:+ rng-p+:expr))
                   (~optional (~seq #:- rng-p-:expr))
                   (~optional (~seq #:object rng-o:expr))))
       (define pre-deps (syntax->list #'(pre-dep-stx ...)))
       (cond
         ;; check for duplicates in arg names
         [(check-duplicates (attribute args.name) free-identifier=?)
          => (λ (id) (parse-error (format "~a appears multiple times as an argument id"
                                          (syntax->datum id))))]
         ;; check for self-reference in dep lists
         [(for/or ([deplist (in-list (attribute args.deps))])
            (and (member (car deplist) (cdr deplist) free-identifier=?)
                 (car deplist)))
          => (λ (arg-id) (parse-error (format "Argument ~a depends on itself."
                                              (syntax->datum arg-id))))]
         ;; check for non-arg names in arg dep lists and #:pre dep list
         [(or (for*/or ([deplist (in-list (attribute args.deps))]
                        [dep (in-list (cdr deplist))])
                (and (not (assoc dep (attribute args.deps) free-identifier=?))
                     dep))
              (for/or ([dep (in-list pre-deps)])
                (and (not (assoc dep (attribute args.deps) free-identifier=?))
                     dep)))
          => (λ (dep-id) (parse-error
                          (format "unknown dependent variable ~a; not an argument of the function"
                                  (syntax->datum dep-id))))]
         ;; check for duplicates in arg dep lists and #:pre dep list
         [(or (for/or ([deplist (in-list (attribute args.deps))])
                (and (check-duplicates (cdr deplist) free-identifier=?)
                     deplist))
              (and (check-duplicates pre-deps free-identifier=?)
                   pre-deps))
          => (λ (deps) (parse-error (format "Repeated identifier(s) in dependency list: ~a"
                                            (map syntax->datum deps))))]
         ;; check for cycles in the dep lists
         [(cycle-in-arg-deps? (attribute args.deps))
          => (λ (cycle) (parse-error
                         (apply string-append
                                " cycle in argument dependencies: "
                                (format "~a depends on ~a"
                                        (syntax->datum (first cycle))
                                        (syntax->datum (second cycle)))
                                (for/list ([x (in-list (cdr cycle))]
                                           [y (in-list (cddr cycle))])
                                  (format ", ~a depends on ~a"
                                          (syntax->datum x)
                                          (syntax->datum y))))))]
         [else
          ;; end of DepFun-specific error checking =)
          ;; let's keep going!
          (define arg-order (arg-deps->idx-order (attribute args.deps)))
          (define arg-type-dict (make-hasheq))
          (for ([idx (in-list arg-order)])
            (define dep-ids (cdr (list-ref (attribute args.deps) idx)))
            (define-values (dep-local-ids dep-local-types)
              (for/lists (_1 _2)
                ([dep-id (in-list dep-ids)])
                (define dep-idx (index-of (attribute args.name) dep-id free-identifier=?))
                (values (list-ref (attribute args.local-name) dep-idx)
                        (hash-ref arg-type-dict dep-idx))))
            (define idx-type
              (with-extended-lexical-env
                  [#:identifiers dep-local-ids
                   #:types dep-local-types]
                (with-local-term-names (map cons dep-ids dep-local-ids)
                  (parse-type (list-ref (attribute args.type-stx) idx)))))

            (hash-set! arg-type-dict idx idx-type))
          (define (abstract rep)
            (abstract-obj rep (attribute args.local-name)))
          (define dom (for/list ([idx (in-range (length arg-order))])
                              (hash-ref arg-type-dict idx)))
          (define abstracted-dom (map abstract dom))
          (with-extended-lexical-env
              [#:identifiers (attribute args.local-name)
               #:types dom]
            (with-local-term-names (map cons
                                        (attribute args.name)
                                        (attribute args.local-name))
              (define pre-prop (parse-prop #'pre-stx))
              (define abstracted-pre-prop (abstract pre-prop))
              (match (parse-values-type #'rng-type)
                ;; single value'd return type, propositions/objects allowed
                [(Values: (list (Result: rng-t _ _)))
                 (define rng-ps (-PS (or (and (attribute rng-p+)
                                              (parse-prop #'rng-p+))
                                         -tt)
                                     (or (and (attribute rng-p-)
                                              (parse-prop #'rng-p-))
                                         -tt)))
                 (define rng-obj (or (and (attribute rng-o)
                                          (parse-obj #'rng-o))
                                     -empty-obj))
                 (define abstracted-rng-t (abstract rng-t))
                 (define abstracted-rng-ps (abstract rng-ps))
                 (define abstracted-rng-obj (abstract rng-obj))
                 (cond
                   [(and (equal? dom abstracted-dom)
                         (equal? rng-t abstracted-rng-t)
                         (TrueProp? abstracted-pre-prop))
                    (make-Fun (list (-Arrow dom (-values abstracted-rng-t
                                                         abstracted-rng-ps
                                                         abstracted-rng-obj))))]
                   [else
                    (make-DepFun
                     abstracted-dom
                     abstracted-pre-prop
                     (-values abstracted-rng-t
                              abstracted-rng-ps
                              abstracted-rng-obj))])]
                ;; multi-valued -- reject if propositions or object present
                [_ #:when (or (attribute rng-p+)
                              (attribute rng-p-)
                              (attribute rng-o))
                   (parse-error
                    #:stx stx
                    "dependent functions returning many values cannot specify latent propositions or objects"
                    "given" (or (and (attribute rng-p+) (format "#:+ ~a" (syntax->datum #'rng-p+)))
                                (and (attribute rng-p-) (format "#:- ~a" (syntax->datum #'rng-p-)))
                                (and (attribute rng-o)  (format "#:object ~a" (syntax->datum #'rng-o)))))]
                [vals
                 (define abstracted-rng-vals (abstract vals))
                 (cond
                   [(and (equal? dom abstracted-dom)
                         (equal? vals abstracted-rng-vals)
                         (TrueProp? abstracted-pre-prop))
                    (make-Fun (list (-Arrow dom vals)))]
                   [else
                    (make-DepFun
                     abstracted-dom
                     abstracted-pre-prop
                     abstracted-rng-vals)])])))])]
      ;; curried function notation
      [((~and dom:non-keyword-ty (~not :->^)) ...
        :->^
        (~and (~seq rest-dom ...) (~seq (~or _ (~between :->^ 1 +inf.0)) ...)))
       (define doms (syntax->list #'(dom ...)))
       (with-arity (length doms)
         (let ([doms (for/list ([d (in-list doms)])
                       (parse-type d))])
           (make-Fun
            (list (-Arrow doms (parse-type (syntax/loc stx (rest-dom ...))))))))]
      [(~or (:->^ dom rng :colon^ latent:legacy-simple-latent-prop)
            (dom :->^ rng :colon^ latent:legacy-simple-latent-prop))
       ;; use parse-type instead of parse-values-type because we need to add the props from the pred-ty
       (with-arity 1
         (make-pred-ty (list (parse-type #'dom)) (parse-type #'rng) (attribute latent.type)
                       (-acc-path (attribute latent.path) (-arg-path 0))))]
      [(~or (:->^ dom:non-keyword-ty ... kws:keyword-tys ... rest:non-keyword-ty ddd:star rng)
            (dom:non-keyword-ty ... kws:keyword-tys ... rest:non-keyword-ty ddd:star :->^ rng))
       (with-arity (length (syntax->list #'(dom ...)))
         (make-Fun
          (list (-Arrow
                 (parse-types #'(dom ...))
                 (parse-values-type #'rng)
                 #:rest (parse-type #'rest)
                 #:kws (map force (attribute kws.Keyword))))))]
      [(~or (:->^ dom:non-keyword-ty ... rest:non-keyword-ty :ddd/bound rng)
            (dom:non-keyword-ty ... rest:non-keyword-ty :ddd/bound :->^ rng))
       (with-arity (length (syntax->list #'(dom ...)))
         (let* ([bnd (syntax-e #'bound)])
           (unless (bound-index? bnd)
             (parse-error
              #:stx #'bound
              "used a type variable not bound with ... as a bound on a ..."
              "variable" bnd))
           (make-Fun
            (list
             (make-arr-dots (parse-types #'(dom ...))
                            (parse-values-type #'rng)
                            (extend-tvars (list bnd)
                                          (parse-type #'rest))
                            bnd)))))]
      [(~or (:->^ dom:non-keyword-ty ... rest:non-keyword-ty _:ddd rng)
            (dom:non-keyword-ty ... rest:non-keyword-ty _:ddd :->^ rng))
       (with-arity (length (syntax->list #'(dom ...)))
         (let ([var (infer-index stx)])
           (make-Fun
            (list
             (make-arr-dots (parse-types #'(dom ...))
                            (parse-values-type #'rng)
                            (extend-tvars (list var) (parse-type #'rest))
                            var)))))]
      #| ;; has to be below the previous one
      [(dom:expr ... :->^ rng)
      (->* (parse-types #'(dom ...))
      (parse-values-type #'rng))]     |#
      ;; use expr to rule out keywords
      [(~or (:->^ dom:non-keyword-ty ... kws:keyword-tys ... rng)
            (dom:non-keyword-ty ... kws:keyword-tys ... :->^ rng))
       (define doms (syntax->list #'(dom ...)))
       (with-arity (length doms)
         (let ([doms (for/list ([d (in-list doms)])
                       (parse-type d))])
           (make-Fun
            (list (-Arrow doms
                          (parse-values-type #'rng)
                          #:kws (map force (attribute kws.Keyword)))))))]
      ;; This case needs to be at the end because it uses cut points to give good error messages.
      [(~or (:->^ ~! dom:non-keyword-ty ... rng:expr
                  :colon^ (~var latent (legacy-full-latent (syntax->list #'(dom ...)))))
            (dom:non-keyword-ty ... :->^ rng:expr
                                ~! :colon^ (~var latent (legacy-full-latent (syntax->list #'(dom ...))))))
       ;; use parse-type instead of parse-values-type because we need to add the props from the pred-ty
       (with-arity (length (syntax->list #'(dom ...)))
         (->* (parse-types #'(dom ...))
              (parse-type #'rng)
              : (-PS (attribute latent.positive) (attribute latent.negative))
              : (attribute latent.object)))]
      [(:->*^ (~var mand (->*-args #t))
              (~optional (~var opt (->*-args #f))
                         #:defaults ([opt.doms null] [opt.kws null]))
              rest:->*-rest
              rng)
       (with-arity (length (attribute mand.doms))
         (define doms (for/list ([d (attribute mand.doms)])
                        (parse-type d)))
         (define opt-doms (for/list ([d (attribute opt.doms)])
                            (parse-type d)))
         (opt-fn doms opt-doms (parse-values-type #'rng)
                 #:rest (and (attribute rest.type)
                             (parse-type (attribute rest.type)))
                 #:kws (map force (append (attribute mand.kws)
                                          (attribute opt.kws)))))]
      [:->^
       (parse-error #:delayed? #t "incorrect use of -> type constructor")
       Err]
      [id:identifier
       (cond
         ;; if it's a type variable, we just produce the corresponding reference (which is in the HT)
         [(bound-tvar? (syntax-e #'id))
          (lookup-tvar (syntax-e #'id))]
         ;; if it was in current-indexes, produce a better error msg
         [(bound-index? (syntax-e #'id))
          (parse-error "type variable must be used with ..."
                       "variable" (syntax-e #'id))]
         ;; if it's a type alias, we expand it (the expanded type is stored in the HT)
         [(lookup-type-alias #'id parse-type (lambda () #f))
          =>
          (lambda (t)
            (when (current-referenced-aliases)
              (define alias-box (current-referenced-aliases))
              (set-box! alias-box (cons #'id (unbox alias-box))))
            (and (syntax-transforming?)
                 (add-disappeared-use (syntax-local-introduce #'id)))
            t)]
         [else
          (parse-error #:delayed? #t (~a "type name `" (syntax-e #'id) "' is unbound"))
          Err])]
      [(:Opaque^ . rest)
       (parse-error "bad syntax in Opaque")]
      [(:U^ . rest)
       (parse-error "bad syntax in Union")]
      [(:Rec^ . rest)
       (parse-error "bad syntax in Rec")]
      [(t ... :->^ . rest)
       (parse-error "bad syntax in ->")]
      [(id arg args ...)
       (let loop
           ([rator (parse-type #'id)]
            [args (parse-types #'(arg args ...))])
         (resolve-app-check-error rator args stx)
         (match rator
           [(? Name?)
            (define app (make-App rator args))
            (register-app-for-checking! app stx)
            app]
           [(Poly: _ _) (instantiate-poly rator args)]
           [(Mu: _ _) (loop (unfold rator) args)]
           [(Error:) Err]
           [_ Err]))]
      [t:atom
       ;; Integers in a "grey area", that is, integers whose runtime type is
       ;; platform-dependent, cannot be safely assigned singleton types.
       ;; Short story: (subtype (-val 10000000000000) -Fixnum) has no safe
       ;;   answer. It's not a fixnum on 32 bits, and saying it's not a fixnum
       ;;   causes issues with type-dead code detection.
       ;; Long story: See email trail for PR13501 and #racket IRC logs from
       ;;   Feb 11 2013.
       (let ([val (syntax-e #'t)])
         (when (and (exact-integer? val)
                    ;; [min-64bit-fixnum, min-portable-fixnum)
                    (or (and (>= val (- (expt 2 62)))
                             (<  val (- (expt 2 30))))
                        ;; (max-portable-index, max-64bit-fixnum]
                        (and (>  val (sub1 (expt 2 28)))
                             (<= val (sub1 (expt 2 62))))))
           (parse-error "non-portable fixnum singleton types are not valid types"
                        "given" val))
         (-val val))]
      [_ (parse-error "expected a valid type"
                      "given" (syntax->datum stx))])))

;; Syntax -> Type
;; Parse a (List ...) type
(define (parse-list-type stx)
  (parameterize ([current-orig-stx stx])
    (syntax-parse stx
      [(:List^ tys ... dty :ddd/bound)
       (let ([var (syntax-e #'bound)])
         (unless (bound-index? var)
           (if (bound-tvar? var)
               (tc-error/stx #'bound "Used a type variable (~a) not bound with ... as a bound on a ..." var)
               (tc-error/stx #'bound "Type variable ~a is unbound" var)))
         (-Tuple* (parse-types #'(tys ...))
                  (make-ListDots
                   (extend-tvars (list var)
                     (parse-type #'dty))
                   var)))]
      [(:List^ tys ... dty _:ddd)
       (let ([var (infer-index stx)])
         (-Tuple* (parse-types #'(tys ...))
                  (make-ListDots
                    (extend-tvars (list var)
                      (parse-type #'dty))
                    var)))]
      [(:List^ tys ...)
       (-Tuple (parse-types #'(tys ...)))])))

;; Syntax -> Type
;; Parse a (Values ...) or AnyValues type
(define (parse-values-type stx)
  (parameterize ([current-orig-stx stx])
    (syntax-parse stx
      [((~or :Values^ :values^) tys ... dty :ddd/bound)
       (let ([var (syntax-e #'bound)])
         (unless (bound-index? var)
           (if (bound-tvar? var)
               (tc-error/stx #'bound "Used a type variable (~a) not bound with ... as a bound on a ..." var)
               (tc-error/stx #'bound "Type variable ~a is unbound" var)))
         (-values-dots (parse-types #'(tys ...))
                       (extend-tvars (list var)
                         (parse-type #'dty))
                       var))]
      [((~or :Values^ :values^) tys ... dty _:ddd)
       (let ([var (infer-index stx)])
         (-values-dots (parse-types #'(tys ...))
                       (extend-tvars (list var)
                         (parse-type #'dty))
                       var))]
      [((~or :Values^ :values^) tys ...)
       (-values (parse-types #'(tys ...)))]
      [:AnyValues^ ManyUniv]
      [t
       (-values (list (parse-type #'t)))])))

;;; Utilities for (Class ...) type parsing

;; process-class-clauses : Option<F> Type Stx FieldDict MethodDict AugmentDict
;;                         -> Option<Id> FieldDict MethodDict AugmentDict
;; Merges #:implements class type and the current class clauses appropriately
(define (merge-with-parent-type row-var parent-type parent-stx fields methods augments)
  ;; merge-clause : Dict Dict -> Dict
  ;; Merge all the non-duplicate entries from the parent types
  (define (merge-clause parent-clause clause)
    (for/fold ([clause clause])
              ([(k v) (in-dict parent-clause)])
      (if (dict-has-key? clause k)
          clause
          (dict-set clause k v))))

  (define (match-parent-type parent-type)
    (define resolved (resolve parent-type))
    (match resolved
      [(Class: row-var _ fields methods augments _)
       (values row-var fields methods augments)]
      [_ (parse-error "expected a class type for #:implements clause"
                      "given" resolved)]))
  (define-values (super-row-var super-fields
                  super-methods super-augments)
    (match-parent-type parent-type))

  (match-define (list (list field-names _) ...) fields)
  (match-define (list (list method-names _) ...) methods)
  (match-define (list (list augment-names _) ...) augments)
  (match-define (list (list super-field-names _) ...) super-fields)
  (match-define (list (list super-method-names _) ...) super-methods)
  (match-define (list (list super-augment-names _) ...) super-augments)

  ;; it is an error for both the extending type and extended type
  ;; to have row variables
  (when (and row-var super-row-var)
    (parse-error (~a "class type with row variable cannot"
                     " extend another type that has a row variable")))

  ;; then append the super types if there were no errors
  (define merged-fields (merge-clause super-fields fields))
  (define merged-methods (merge-clause super-methods methods))
  (define merged-augments (merge-clause super-augments augments))

  ;; make sure augments and methods are disjoint
  (define maybe-dup-method (check-duplicates (dict-keys merged-methods)))
  (when maybe-dup-method
    (parse-error "duplicate method name" "name" maybe-dup-method))
  (define maybe-dup-augment (check-duplicates (dict-keys merged-augments)))
  (when maybe-dup-augment
    (parse-error "duplicate augmentable method name"
                 "name" maybe-dup-augment))

  (values (or row-var super-row-var) merged-fields
          merged-methods merged-augments))

;; Syntax -> Type
;; Parse a (Object ...) type
;; This is an alternative way to write down an Instance type
(define (parse-object-type stx)
  (syntax-parse stx
    [(kw clause:object-type-clauses)
     (add-disappeared-use #'kw)
     (define fields (map list
                         (stx-map syntax-e #'clause.field-names)
                         (stx-map parse-type #'clause.field-types)))
     (define methods (map list
                          (stx-map syntax-e #'clause.method-names)
                          (stx-map parse-type #'clause.method-types)))
     (check-function-types methods)
     (make-Instance (make-Class #f null fields methods null #f))]))

;; Syntax -> Type
;; Parse a (Row ...), which are used in `row-inst`.
(define (parse-row stx)
  (syntax-parse stx
    [(Row^: (~var clause (row-type-clauses parse-type)))
     (add-disappeared-use #'Row)
     (define inits (attribute clause.inits))
     (define fields (attribute clause.fields))
     (define methods (attribute clause.methods))
     (define augments (attribute clause.augments))
     (define init-rest
       (and (attribute clause.init-rest)
            (parse-type (attribute clause.init-rest))))
     (check-function-types methods)
     (check-function-types augments)
     (make-Row inits fields methods augments init-rest)]
  [_ (parse-error "expected a valid row"
                  "given" (syntax->datum stx))]))

;; Syntax -> Type
;; Parse a (Class ...) type
(define (parse-class-type stx)
  (syntax-parse stx
    [(kw (~var clause (class-type-clauses parse-type)))
     (add-disappeared-use #'kw)
     (define parent-stxs (stx->list #'clause.implements))
     (define parent/init-stx (attribute clause.implements/inits))
     (define parent/init-type (and parent/init-stx (parse-type parent/init-stx)))
     (define parent-types
       (let ([types (map parse-type parent-stxs)])
         (if parent/init-stx
             (cons parent/init-type types)
             types)))
     (define given-inits (attribute clause.inits))
     (define given-fields (attribute clause.fields))
     (define given-methods (attribute clause.methods))
     (define given-augments (attribute clause.augments))
     (define given-row-var
       (and (attribute clause.row-var)
            (parse-type (attribute clause.row-var))))
     (define given-init-rest
       (and (attribute clause.init-rest)
            (parse-type (attribute clause.init-rest))))

     (cond ;; If an Error type flows into the #:row-var position, a
           ;; delayed error should be raised from the recursive call to
           ;; `parse-type` so no additional error is needed here.
           [(Error? given-row-var) Err]
           [(and given-row-var (not (F? given-row-var)))
            (parse-error "expected a type variable for #:row-var"
                         "given" given-row-var)
            Err]
           ;; Only proceed to create a class type when the parsing
           ;; process isn't looking for recursive type alias references.
           ;; (otherwise the merging process will error)
           [(or (and (null? parent-stxs) (not parent/init-stx))
                (not (current-referenced-aliases)))

            (check-function-types given-methods)
            (check-function-types given-augments)

            ;; merge with all given parent types, erroring if needed
            (define-values (row-var fields methods augments)
              (for/fold ([row-var given-row-var]
                         [fields given-fields]
                         [methods given-methods]
                         [augments given-augments])
                  ([parent-type (reverse parent-types)]
                   [parent-stx  (reverse (append (or (list parent/init-stx) null)
                                                 parent-stxs))])
                (merge-with-parent-type row-var parent-type parent-stx
                                        fields methods augments)))

            ;; check constraints on row var for consistency with class
            (when (and row-var (has-row-constraints? (F-n row-var)))
              (define constraints (lookup-row-constraints (F-n row-var)))
              (check-constraints given-inits (car constraints))
              (check-constraints fields (cadr constraints))
              (check-constraints methods (caddr constraints))
              (check-constraints augments (cadddr constraints)))

            ;; For the #:implements/inits entry, put the inits into the type
            ;; as well. They are appended at the end to match the runtime behavior
            ;; of init arguments.
            (define parent-inits (get-parent-inits parent/init-type))

            (define class-type
              (make-Class row-var
                          (append given-inits parent-inits)
                          fields methods augments given-init-rest))

            class-type]
           [else
            ;; Conservatively assume that if there *are* #:implements
            ;; clauses, then the current type alias will be recursive
            ;; through one of the type aliases in the #:implements clauses.
            ;;
            ;; This is needed because it's hard to determine if a type
            ;; in the #:implements clauses depends on the current
            ;; type alias at this point. Otherwise, we would have to
            ;; parse all type aliases again.
            ;;
            ;; An example type that is a problem without this assumption is
            ;;   alias = (Class #:implements Foo%) where Foo%
            ;;           has a class clause referring to alias
            ;; since "alias" will be a non-recursive alias
            ;;
            ;; Without the approximation, we may miss recursive references
            ;; which can cause infinite looping elsewhere in TR.
            ;;
            ;; With the approximation, we have spurious recursive references
            ;; which may cause more indirection through the Name environment
            ;; or generate worse contracts.
            (define alias-box (current-referenced-aliases))
            (set-box! alias-box (cons (current-type-alias-name)
                                      (unbox alias-box)))
            (define class-box (current-referenced-class-parents))
            (set-box! class-box (append (if parent/init-stx
                                            (cons parent/init-stx parent-stxs)
                                            parent-stxs)
                                        (unbox class-box)))
            ;; Ok to return Error here, since this type will
            ;; get reparsed in another pass
            Err])]))

;; get-parent-inits : (U Type #f) -> Inits
;; Extract the init arguments out of a parent class type
(define (get-parent-inits parent)
  (cond [(not parent) null]
        [else
         (define resolved (resolve parent))
         (match resolved
           [(Class: _ inits _ _ _ _) inits]
           [_ (parse-error "expected a class type for #:implements/inits clause"
                           "given" resolved)])]))

;; check-function-types : Dict<Name, Type> -> Void
;; ensure all types recorded in the dictionary are function types
(define (check-function-types method-types)
  ;; TODO: this function should probably go in a utility
  ;;       module since it's duplicated elsewhere
  (define (function-type? type)
    (match (resolve type)
      [(? Fun?) #t]
      [(Poly: _ body) (function-type? body)]
      [(PolyDots: _ body) (function-type? body)]
      [(PolyRow: _ _ body) (function-type? body)]
      [_ #f]))
  (for ([(id pre-type) (in-dict method-types)])
    (define type (car pre-type))
    (unless (function-type? type)
      (parse-error "method must have a function type"
                   "method name" id
                   "given type" type))))

;; check-constraints : Dict<Name, _> Listof<Name> -> Void
;; helper to check if the constraints are consistent with the type
(define (check-constraints type-table constraint-names)
  (define names-from-type (dict-keys type-table))
  (define conflicting-name
    (for/or ([m (in-list names-from-type)])
      (and (not (memq m constraint-names))
           m)))
  (when conflicting-name
    (parse-error "class member conflicts with row variable constraints"
                 "conflicting name" conflicting-name)))

(define (parse-tc-results stx)
  (syntax-parse stx
    [((~or :Values^ :values^) t ...)
     (define empties (stx-map (λ (x) #f) #'(t ...)))
     (ret (parse-types #'(t ...))
          empties
          empties)]
    [:AnyValues^ (-tc-any-results #f)]
    [t (ret (parse-type #'t) #f #f)]))

(define parse-type/id (parse/id parse-type))

;; parse-error : String String String ... ... -> Void
;; helper for parse-type error messages
(define (parse-error reason
                     #:delayed? [delayed? #f]
                     #:stx [stx (current-orig-stx)]
                     . rst)
  (apply tc-error/fields "parse error in type"
                         #:more reason
                         #:delayed? delayed?
                         rst))
