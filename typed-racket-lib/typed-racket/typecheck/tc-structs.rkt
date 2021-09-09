#lang racket/base

(require "../utils/utils.rkt"
         syntax/struct syntax/parse racket/function racket/match racket/list syntax/id-set
         racket/format syntax/id-table
         (prefix-in c: (contract-req))
         "../rep/type-rep.rkt"
         "../rep/type-constr.rkt"
         "struct-type-constr.rkt"
         "../rep/free-variance.rkt"
         "../rep/values-rep.rkt"
         "../private/parse-type.rkt"
         "../private/syntax-properties.rkt"
         "../types/base-abbrev.rkt"
         "../types/abbrev.rkt"
         "../types/subtype.rkt"
         "../types/utils.rkt"
         "../types/resolve.rkt"
         "../types/substitute.rkt"
         "../types/struct-table.rkt"
         "../env/global-env.rkt"
         "../env/type-name-env.rkt"
         "../env/type-alias-env.rkt"
         "../env/type-constr-env.rkt"
         "../env/tvar-env.rkt"
         "../env/lexical-env.rkt"
         "../env/struct-name-env.rkt"
         "../utils/tc-utils.rkt"
         "../utils/prefab.rkt"
         "../utils/identifier.rkt"
         (only-in "../utils/struct-info.rkt" maybe-struct-info-wrapper-type)
         "typechecker.rkt"
         "def-binding.rkt"
         "internal-forms.rkt"
         "error-message.rkt"
         "tc-subst.rkt"
         "renamer.rkt"
         (only-in "../infer/infer.rkt" intersect)
         (for-syntax syntax/parse racket/base)
         (for-template racket/base))

(require-for-cond-contract racket/struct-info)

(provide tc/struct
         tc/struct-prop-values
         tc/make-struct-type-property
         name-of-struct d-s
         refine-struct-variance!
         register-parsed-struct-sty!
         register-parsed-struct-bindings!)

(define-syntax-class parent
  #:attributes (name par)
  (pattern (name:id par:id))
  (pattern name:id #:attr par #f))

;; sty : (U Struct? Prefab?)
;; names : struct-names
;; desc : struct-desc
;; struct-info : struct-info?
;; type-only : Boolean
(struct parsed-struct (sty names desc struct-info type-only) #:transparent)

;; struct-name : Id  (the identifier for the static struct info,
;;                    usually the same as the type-name)
;; type-name : Id    (the identifier for the type name)
;; struct-type : Id  (the identifier for the struct type binding)
;; constructor : Id
;; extra-constructor : (Option Id)
;; predicate : Id
;; getters : Listof[Id]
;; setters : Listof[Id] or #f
(struct struct-names (struct-name type-name struct-type constructor extra-constructor predicate getters setters) #:transparent)

;; struct-desc holds all the relevant information about a struct type's types
;; parent-fields : (Listof Type)
;; self-fields : (Listof Type)
;; tvars : (Listof Symbol)
;; mutable: Any
;; parent-mutable: Any
(struct struct-desc (parent-fields self-fields tvars
                     mutable parent-mutable)
        #:transparent)

(define (struct-desc-all-fields fields)
  (append (struct-desc-parent-fields fields) (struct-desc-self-fields fields)))
(define (struct-desc-parent-count fields)
  (length (struct-desc-parent-fields fields)))

(define (name-of-struct stx)
  (syntax-parse stx
    [t:typed-struct #'t.type-name]))

;; [base struct type name] ->  Listof[its sub struct type name]
(define inheritance-tbl (make-free-id-table))

(define struct-def-at-top-level? (make-parameter #f))

;; [struct type name] ->  Listof[its constructor name]
(define constructor-tbl (make-free-id-table))

;; a simple wrapper to get proc from a polymorphic or monomorhpic structure
(define/cond-contract (get-struct-proc sty)
  (c:-> (c:or/c Struct? Poly?) (c:or/c #f Fun?))
  (Struct-proc (match sty
                 [(? Struct?) sty]
                 [(Poly: names (? Struct? sty)) sty])))

;; If the a struct has a base struct defined in the enclose module, record the
;; substructuring relation in the inheritance table.
(define/cond-contract (register-substructuring! maybe-base-name-rep maybe-base-struct-rep substruct-type-name substruct-set-procedural?)
  (c:-> (c:or/c identifier? #f) (c:or/c #f Struct? Poly?) identifier? boolean? void?)
  ;; given where register-substructuring! is called currently,
  ;; maybe-base-name-rep and maybe-base-struct-rep should be #f or non-false
  ;; value at the same time
  (when (and maybe-base-name-rep (not substruct-set-procedural?))
    (define name-id maybe-base-name-rep)
    ;; if a base struct rep's proc is already set, it means the base struct is
    ;; imported and the type checker doesn't need to deal with its properties in
    ;; pass 2, thus there is no need to put it in the table.
    (unless (get-struct-proc maybe-base-struct-rep)
      (free-id-table-update! inheritance-tbl
                             name-id
                             (lambda (substruct-type-names)
                               (cons substruct-type-name substruct-type-names))
                             null))))

;; make a struct and its substructs procedural
(define/cond-contract (mk-substructs-procedural! base-tname base-sty new-ty)
  (c:-> identifier? Struct? Fun? void?)
  (define substuct-tnames (free-id-table-ref inheritance-tbl base-tname null))
  (for ([stn (in-list substuct-tnames)])
    (define sty (lookup-type-name stn))
    (mk-substructs-procedural! stn sty new-ty))
  (mk-struct-procedural! base-tname base-sty new-ty))


(define/cond-contract (mk-struct-procedural! stname sty new-ty)
  (c:-> identifier? Struct? Fun? void?)

  (define (get-struct-type type)
    (match type
      [(Intersection: ts _)
       (for/first ([ty (in-list ts)]
                   #:when (Struct? (resolve ty)))
         ty)]
      [(and (? Name?) (app resolve (? Struct?))) type]))

  (Struct-update-proc! sty new-ty)
  (for ([constr-name (in-list (free-id-table-ref constructor-tbl stname))])
    (match (single-value constr-name)
      [(tc-result1: (Fun: (list (and (Arrow: _ _ _ (Values: (list (Result: (and (app get-struct-type st-type-name))
                                                                           prop obj)))) arr))))
       (register-type constr-name
                      (make-Fun (list (Arrow-update arr rng
                                                    (lambda (val)
                                                      (Values-update val results
                                                                     (lambda (res)
                                                                       (list (make-Result (intersect st-type-name new-ty) prop obj)))))))))])))


(define/cond-contract (tc/struct-prop-values st-tname pnames pvals)
  (c:-> identifier? (c:listof identifier?) (c:listof syntax?) void?)
  (unless (null? pnames)
    (define sty (lookup-type-name st-tname))
    (for ([p (in-list pnames)]
          [pval (in-list pvals)])
      (parameterize ([current-orig-stx pval])
        (cond
          [(equal? (syntax-e p) 'prop:procedure)
           (syntax-parse pval
             [(quote proc-fld:exact-nonnegative-integer)
              (define flds (Struct-subflds sty))
              (define idx (syntax->datum #'proc-fld))
              (cond
                [(<= (length flds) idx)
                 (tc-error/fields "index too large"
                                  "index" idx
                                  "maximum allowed index" (sub1 (length flds)))]
                [else
                 (define fld (list-ref flds idx))
                 (define fld-ty (fld-t fld))
                 (cond
                   [(Fun? fld-ty)
                    (mk-substructs-procedural! st-tname sty fld-ty)]
                   [else
                    (tc-error/fields "type mismatch in the field for prop:procedure"
                                     "expected" "Procedure"
                                     "given" fld-ty)])])]
             [_
              (match (single-value pval)
                [(tc-result1: (Fun: (list (and (Arrow: (list dom-mes domss ...) _ _ _) arr-li) ...)))
                 (define maybe-new-ty (for/fold ([ok? #t]
                                                 [new-arr-li null]
                                                 #:result (if ok? (make-Fun (reverse new-arr-li))
                                                              #f))
                                                ([me (in-list dom-mes)]
                                                 [rst-doms (in-list domss)]
                                                 [arr (in-list arr-li)])
                                        (cond
                                          [(not (type-equiv? me sty))
                                           (tc-error/fields "type mismatch in the domain of the value for prop:procedure"
                                                            "expected" sty
                                                            "got" me)
                                           (values #f new-arr-li)]
                                          [else
                                           (values ok?
                                                   (cons (Arrow-update arr dom (lambda _ rst-doms))
                                                         new-arr-li))])))
                 (cond
                   [maybe-new-ty
                    (mk-substructs-procedural! st-tname sty maybe-new-ty)]
                   [else (void)])]
                [(tc-result1: ty) (expected-but-got "Procedure or nonnegative integer literal" ty)])])]
          [else
           (match (single-value p)
             [(tc-result1: (Struct-Property: ty _))
              (match-define (F: var) -Self)
              (match-define (F: var-imp) -Imp)
              (match sty
                [(? Struct?)
                 (tc-expr/check pval (ret (subst var-imp sty (subst var sty ty))))]
                [(Poly-names: names sty)
                 (let* ([v (subst var sty ty)]
                        [v (for/fold ([res sty]
                                      #:result (subst var-imp res v))
                                     ([n names])
                             (subst n (make-F (gensym n)) res))]
                        [v (ret v)])
                   (extend-tvars names (tc-expr/check pval v)))])]
             [(tc-result1: ty)
              (tc-error "expected a struct type property but got ~a" ty)])
           (void)])))))


;; parse name field of struct, determining whether a parent struct was specified
(define/cond-contract (parse-parent nm/par prefab?)
  (c:-> syntax? c:any/c (values identifier? (c:or/c identifier? #f) (c:or/c Poly? Struct? TypeConstructor? Prefab? #f)))
  (syntax-parse nm/par
    [v:parent
     (if (attribute v.par)
         (let* ([parent0 (cond
                            ;; maybe-struct-info-wrapper-type only returns
                            ;; parent's type name when the parent structure
                            ;; defined either is in a typed module or
                            ;; imported via require/typed
                            [(lookup-struct-name #'v.par)]
                            [(maybe-struct-info-wrapper-type (syntax-local-value #'v.par (lambda () #f)))]
                            ;; if the parent struct is a builtin structure like exn,
                            ;; its structure name is also its type name.
                            [else #'v.par])]
                [parent (let loop ((parent (parse-type-or-type-constructor parent0)))
                          (match parent
                            [(? Name?) (loop (resolve-name parent))]
                            [(or (? TypeConstructor?) (? Struct?)) parent]
                            [(? Prefab?) #:when prefab?
                                         parent]
                            [_
                             (tc-error/stx #'v.par "parent type not a valid structure name: ~a"
                                           (syntax->datum #'v.par))]))])
           (values #'v.name parent0 parent))
         (values #'v.name #f #f))]))


;; generate struct names given type name, field names
;; and optional constructor name
;; all have syntax loc of name
;; identifier listof[identifier] Option[identifier] -> struct-names
(define (get-struct-names type-name nm flds maker* extra-maker)
  (define (split l)
    (let loop ([l l] [getters '()] [setters '()])
      (if (null? l)
          (values (reverse getters) (reverse setters))
          (loop (cddr l) (cons (car l) getters) (cons (cadr l) setters)))))
  (match (build-struct-names nm flds #f #f nm #:constructor-name maker*)
    [(list sty maker pred getters/setters ...)
     (let-values ([(getters setters) (split getters/setters)])
       (struct-names nm type-name sty maker extra-maker pred getters setters))]))

;; gets the fields of the parent type, if they exist
;; Option[Struct-Ty] -> Listof[Type]
(define/cond-contract (get-flds p)
  (c:-> (c:or/c Struct? #f) (c:listof fld?))
  (match p
    [(Struct: _ _ flds _ _ _ _) flds]
    [#f null]))


;; Constructs the Struct value for a structure type
;; The returned value has free type variables
(define/cond-contract (mk/inner-struct-type names desc parent [property-names empty])
  (c:->* (struct-names? struct-desc? (c:or/c Struct? #f)) ((c:listof identifier?)) Struct?)

  (let* ([this-flds (for/list ([t (in-list (struct-desc-self-fields desc))]
                               [g (in-list (struct-names-getters names))])
                       (make-fld t g (struct-desc-mutable desc)))]
         [flds (append (get-flds parent) this-flds)])
    (make-Struct (struct-names-struct-name names)
                 parent
                 flds
                 (and parent (Struct-proc parent))
                 (not (null? (struct-desc-tvars desc)))
                 (struct-names-predicate names)
                 (immutable-free-id-set property-names))))


;; construct all the various types for structs, and then register the appropriate names
(define/cond-contract (register-sty! sty names desc)
  (c:-> (c:or/c Struct? Prefab?) struct-names? struct-desc? void?)

  ;; a type alias needs to be registered here too, to ensure
  ;; that parse-type will map the identifier to this Name type
  (define type-name (struct-names-type-name names))
  (define struct-name (struct-names-struct-name names))
  (define (register-alias alias)
    (register-resolved-type-alias alias
                                  (make-Name type-name
                                             (length (struct-desc-tvars desc))
                                             (Struct? sty))))
  (register-alias type-name)
  (unless (free-identifier=? type-name struct-name)
    (register-struct-name! struct-name type-name))

  ;; a polymorphic structure makes its name represent two things:
  ;; the polymorphic stucture type, e.g. foo is an alias to (All (a) (foo a)); (is this really true?/useful)
  ;; a type constructor to create the monomophic structure types of foo. e.g (foo Integer)
  (register-type-name type-name
                      (make-Poly (struct-desc-tvars desc) sty))
  (define sty^ (make-Poly (struct-desc-tvars desc) sty))
  (unless (empty? (struct-desc-tvars desc))
    (define ty-op (make-type-constr (struct-type-op sty^)
                                (length (struct-desc-tvars desc))))
    (register-type-constructor! type-name ty-op)))

;; Register the appropriate types, return a list of struct bindings
(define/cond-contract (register-struct-bindings! sty names desc si)
  (c:-> (c:or/c Struct? Prefab?) struct-names? struct-desc? (c:or/c #f struct-info?) (c:listof binding?))
  (match sty
    [(? Struct?) (register-non-prefab-bindings! sty names desc si)]
    [(? Prefab?) (register-prefab-bindings! sty names desc si)]))

;; a helper predicate function for values created by mk-type-alias
(define (type-alias? a)
  (or (Name? a) (App? a) (Intersection? a)))

;; Generates an alias of the (struct) type that the `type-name` references.  The
;; alias is mainly used to help create the types of the constructor, field
;; accessors and mutators etc.  If the original (struct) type is monomorphic,
;; i.e. `tvars` is null, the alias is a `Name`. Otherwise, it is an `App`
;; to encode (StructTypeName tvar ...)
(define/cond-contract (mk-type-alias type-name tvars maybe-proc-ty)
  (c:-> identifier? (c:listof symbol?) (c:or/c #f Fun?) type-alias?)
  (define name-type (make-Name type-name (length tvars) #t))
  (define alias^ (if (null? tvars)
                     name-type
                     (make-App name-type (map make-F tvars))))
  (if maybe-proc-ty (intersect alias^ maybe-proc-ty)
      alias^))

(define/cond-contract (register-prefab-bindings! pty names desc si)
  (c:-> Prefab? struct-names? struct-desc? (c:or/c #f struct-info?) (c:listof binding?))
  (define key (Prefab-key pty))
  (match-define (struct* struct-desc ([mutable self-mutable] [parent-mutable parent-mutable])) desc)
  (define field-count (length (struct-desc-all-fields desc)))
  (define field-univs (build-list field-count (λ (_) Univ)))
  (define field-tvar-syms (build-list field-count (λ (_) (gen-pretty-sym))))
  (define field-tvar-Fs (map make-F field-tvar-syms))
  (define raw-poly-prefab (make-Prefab key field-tvar-Fs)) ;; since all prefabs are polymorphic by nature

  (define prefab-top-type (make-PrefabTop key))

  (mk-bindings! pty names desc si
                (lambda (_) prefab-top-type)
                (lambda (acc-id t idx _)
                  (define path (make-PrefabPE key idx))
                  (define fld-sym (list-ref field-tvar-syms idx))
                  (define fld-t (list-ref field-tvar-Fs idx))
                  (define func-t (cond
                                   [(or self-mutable parent-mutable)
                                    ;; NOTE - if we ever track mutable fields more granularly
                                    ;; than "all of the fields are mutable or not" then this
                                    ;; could be more precise (i.e. include the path elem
                                    ;; for any immutable field).
                                    (make-Poly field-tvar-syms (cl-> [(raw-poly-prefab) fld-t]
                                                                     [(prefab-top-type) Univ]))]
                                   [else
                                    (make-Poly (list fld-sym)
                                               (cl->*
                                                (->acc (list (make-Prefab key (list-set field-univs idx fld-t)))
                                                       fld-t
                                                       (list path))
                                                (-> prefab-top-type Univ)))]))
                  (values (list prefab-top-type idx self-mutable #t) func-t))
                (lambda (s t idx _)
                  (define fld-t (list-ref field-tvar-Fs idx))
                  (values (list prefab-top-type idx #t)
                          (make-Poly field-tvar-syms (->* (list raw-poly-prefab fld-t) -Void))))))

(define/cond-contract (register-non-prefab-bindings! sty names desc si)
  (c:-> Struct? struct-names? struct-desc? (c:or/c #f struct-info?) (c:listof binding?))
  (match-define (struct-desc parent-fields self-fields tvars mutable parent-mutable) desc)

  ;; is this structure covariant in *all* arguments?
  (define (covariant-for? fields mutable)
    (for*/and ([var (in-list tvars)]
               [t (in-list fields)])
      (let ([variance (hash-ref (free-vars-hash (free-vars* t)) var variance:const)])
        (or (variance:const? variance)
            (and (not mutable) (variance:co? variance))))))

  (define covariant?
    (and (covariant-for? self-fields mutable)
         (covariant-for? parent-fields parent-mutable)))

  (define (poly-wrapper t) (make-Poly tvars t))
  (mk-bindings! sty names desc si
                (lambda (st-type-alias)
                  (if (not covariant?)
                      (make-StructTop sty)
                      (subst-all (make-simple-substitution tvars (map (const Univ) tvars)) st-type-alias)))
                (lambda (name self-fld idx-parent-cnt st-type-alias)
                  (let* ([path (make-StructPE st-type-alias idx-parent-cnt)]
                         [func (poly-wrapper
                                (if mutable
                                    (->* (list st-type-alias) self-fld)
                                    (->acc (list st-type-alias) self-fld (list path))))])
                    (values (list st-type-alias idx-parent-cnt mutable #f) func)))
                (lambda (_ self-fld idx-parent-cnt st-type-alias)
                  (values (list st-type-alias idx-parent-cnt #f) (poly-wrapper (->* (list st-type-alias self-fld) -Void))))))

;; if val is not false, cons it onto rst
;; (-> (Option A) [(Listof A)] (Listof A))
(define (maybe-cons val [rst null])
  (if val
      (cons val rst)
      rst))

;; creates bindings of the descriptor, predicate, field accessors, field
;; mutators (if any), constructor, extra constructor (if any) of a prefab or
;; non-prefab structure type as well as an auxilary def-struct-stx-binding for
;; handling the struct info. The customization for prefabs and non-prefabs is
;; provided through these three mk-* parameters.
;;
;; sty - the type rep of the structure.
;; names - a struct-names for the structure
;; desc - a struct-desc for the structure
;; si - the struct info
;;
;; mk-pred-ty - a function that takes a type-alias? value and produces the
;; predicate type
;;
;; mk-getter-vals/mk-setter-vals - a function that takes four arguments: the
;; field accessor name, the field type, the index of parent field and a
;; type-alias? value. Both function need to return a pair of values. The first one
;; is a list of values later supplied as all but the first arguments in order to
;; add-struct-accessor-fn!/add-struct-mutator-fn!. The other is the type of the
;; field getter/setter.
(define/cond-contract (mk-bindings! sty names desc si mk-pred-ty mk-getter-vals mk-setter-vals)
  (c:-> (c:or/c Struct? Prefab?) struct-names? struct-desc? (c:or/c #f struct-info?)
        (c:-> type-alias? Type?)
        (c:-> identifier? Type? exact-nonnegative-integer? type-alias?
              (values (c:list/c Type? exact-nonnegative-integer? boolean? boolean?)
                      Type?))
        (c:-> identifier? Type? exact-nonnegative-integer? type-alias?
              (values (c:list/c Type? exact-nonnegative-integer? boolean?)
                      Type?))
        (c:listof binding?))
  (match-define (struct* struct-desc
                         ([self-fields self-fields]
                          [tvars tvars]
                          [mutable self-mutable]))
    desc)

  (match-define (struct-names struct-name type-name struct-type constructor-name extra-constructor predicate getters setters) names)

  (define all-fields (struct-desc-all-fields desc))
  (define parent-count (struct-desc-parent-count desc))

  ;; the alias, with free type variables
  (define st-type-alias (let ([maybe-proc-ty (and (or (Poly? sty) (Struct? sty))
                                                  (get-struct-proc sty))])
                          (mk-type-alias type-name tvars maybe-proc-ty)))

  ;; simple abstraction for handling field getters or setters
  ;; operators - names of field getters or getters
  ;; add-struct-accessor-fn! - (or add-struct-accessor-fn! add-struct-mutator-fn!)
  ;; mk-vals - (or mk-getter-vals mk-setter-vals)
  (define (mk-operator-bindings! operators add-struct-operator-fn! mk-vals)
    (for/list ([opname (in-list operators)]
               [self-fld (in-list self-fields)]
               [idx-parent-cnt (in-naturals parent-count)])
      (let-values ([(fn-args poly-ty) (mk-vals opname self-fld idx-parent-cnt st-type-alias)])
        (apply add-struct-operator-fn! opname fn-args)
        (make-def-binding opname poly-ty))))

  (define bindings
    (list* (make-def-binding struct-type (make-StructType sty))
           (make-def-binding predicate
                             (make-pred-ty (mk-pred-ty st-type-alias)))
           (append*
            (mk-operator-bindings! getters add-struct-accessor-fn! mk-getter-vals)
            (maybe-cons (and self-mutable (mk-operator-bindings! setters add-struct-mutator-fn! mk-setter-vals))))))

  (define constructor-type (make-Poly tvars (->* all-fields st-type-alias)))

  (define struct-binding (make-def-struct-stx-binding struct-name
                                                      struct-name
                                                      type-name
                                                      si
                                                      constructor-name
                                                      constructor-type
                                                      extra-constructor))
  (define def-bindings
    (maybe-cons (and extra-constructor (make-def-binding extra-constructor constructor-type))
                bindings))

  (free-id-table-set! constructor-tbl type-name (maybe-cons extra-constructor (list constructor-name)))

  (define constructor-binding (make-def-binding constructor-name constructor-type))
  (for ([b (in-list (cons constructor-binding def-bindings))])
    (register-type (binding-name b) (def-binding-ty b)))

  (maybe-cons (and (not (free-identifier=? constructor-name struct-name)) constructor-binding)
              (cons
               struct-binding
               (maybe-cons (and (not (free-identifier=? type-name struct-name))
                                ;; since type-name is also an syntax transformer that contains the
                                ;; struct info, we generate a struct stx binding for it here
                                (make-def-struct-stx-binding type-name
                                                             struct-name
                                                             type-name
                                                             si
                                                             constructor-name
                                                             constructor-type
                                                             extra-constructor))
                           def-bindings))))

(define (register-parsed-struct-sty! ps)
  (match ps
    ((parsed-struct sty names desc si type-only)
     (register-sty! sty names desc))))

(define (register-parsed-struct-bindings! ps)
  (match ps
    ((parsed-struct sty names desc si type-only)
     (if type-only
         null
         (register-struct-bindings! sty names desc si)))))

;; Listof<Parsed-Struct> -> Void
;; Refines the variance of struct types in the name environment
(define (refine-struct-variance! parsed-structs)
  (define stys (map parsed-struct-sty parsed-structs))
  (define tvarss (map (compose struct-desc-tvars parsed-struct-desc) parsed-structs))
  (define names
    (for/list ([parsed-struct (in-list parsed-structs)])
      (struct-names-type-name (parsed-struct-names parsed-struct))))
  (refine-variance! names stys tvarss))

;; check and register types for a define struct
;; tc/struct : Listof[identifier] (U identifier (list identifier identifier))
;;             Listof[identifier] Listof[syntax]
;;             -> void
(define (tc/struct vars nm/par type-name fld-names tys
                   #:maker [maker #f]
                   #:extra-maker [extra-maker #f]
                   #:mutable [mutable #f]
                   #:type-only [type-only #f]
                   #:prefab? [prefab? #f]
                   #:properties [properties empty])
  (define-values (nm parent-name parent^) (parse-parent nm/par prefab?))
  ;; create type variables for the new type parameters
  (define tvars (map syntax-e vars))
  (define new-tvars (map make-F tvars))
  (define parent (match parent^
                   [(struct* TypeConstructor ([arity expected]))
                    (define given (length new-tvars))
                    (unless (<= expected given)
                      (tc-error (~a "wrong number of arguments to type constructor"
                                    "\n  type constructor: " (syntax-e parent-name)
                                    "\n  expected: " expected
                                    "\n  given: " given
                                    "\n  arguments...: " new-tvars)))
                    (apply parent^ (take new-tvars expected))]
                   [else
                    parent^]))

  (unless prefab?
    (register-substructuring! parent-name parent type-name
                              (ormap (lambda (prop)
                                       (equal? (syntax-e prop) 'prop:procedure))
                                     properties)))
  ;; parse the types
  (define types
    ;; add the type parameters of this structure to the tvar env
    (extend-tvars tvars
                  (parameterize ([current-poly-struct `#s(poly ,type-name ,new-tvars)])
                    ;; parse the field types
                    (map parse-type tys))))
  ;; instantiate the parent if necessary, with new-tvars
  (define concrete-parent
    (if (Poly? parent)
        (if (> (Poly-n parent) (length new-tvars))
            (tc-error "Could not instantiate parent struct type. Required ~a type variables, received ~a."
                      (Poly-n parent)
                      (length new-tvars))
            (instantiate-poly parent (take new-tvars (Poly-n parent))))
        parent))
  ;; create the actual structure type, and the types of the fields
  ;; that the outside world will see
  ;; then register it
  (define names (get-struct-names type-name nm fld-names maker extra-maker))

  (cond [prefab?
         (define-values (parent-key parent-fields)
           (match concrete-parent
             [#f (values null null)]
             [(Prefab: parent-key parent-fields)
              (values parent-key parent-fields)]))
         (define key-prefix
           (if mutable
               (list (syntax-e nm)
                     (length fld-names)
                     (build-vector (length fld-names) values))
               (list (syntax-e nm))))
         (define key
           (normalize-prefab-key (append key-prefix parent-key)
                                 (+ (length fld-names) (length parent-fields))))
         (define parent-mutable
           (match parent-key
             [(list-rest _ num-fields _ mutable _)
              (= num-fields (vector-length mutable))]
             ['() #f]))
         (define desc
           (struct-desc parent-fields types tvars mutable parent-mutable))
         (parsed-struct (make-Prefab key (append parent-fields types))
                        names desc (struct-info-property nm/par) #f)]
        [else
         (define parent-mutable
           ;; Only valid as long as typed structs must be
           ;; either fully mutable or fully immutable
           (or (not parent)
               (andmap fld-mutable? (get-flds concrete-parent))))

         (define desc (struct-desc (map fld-t (get-flds concrete-parent))
                                   types tvars
                                   mutable parent-mutable))

         (define sty (mk/inner-struct-type names desc concrete-parent properties))

         (parsed-struct sty names desc (struct-info-property nm/par) type-only)]))

;; register a struct type
;; convenience function for built-in structs
;; FIXME - figure out how to make this lots lazier
(define/cond-contract (tc/builtin-struct nm parent fld-names tys kernel-maker)
  (c:-> identifier? (c:or/c #f identifier?) (c:listof identifier?)
        (c:listof Type?) (c:or/c #f identifier?)
        c:any/c)
  (define parent-type
    (and parent (resolve-name (make-Name parent 0 #t))))
  (define parent-tys (map fld-t (get-flds parent-type)))

  (define names (get-struct-names nm nm fld-names #f #f))
  ;; built-in structs are assumed to be immutable with immutable parents
  (define desc (struct-desc parent-tys tys null #f #f))
  (define sty (mk/inner-struct-type names desc parent-type))

  (register-sty! sty names desc)
  (void ;; need the `register-type` side effect, but not the output bindings
    (register-struct-bindings! sty names desc #f))
  (when kernel-maker
    (register-type kernel-maker (λ () (->* (struct-desc-all-fields desc) sty)))))

;; syntax for tc/builtin-struct
(define-syntax (d-s stx)
  (define-splicing-syntax-class options
    (pattern (~optional (~seq #:kernel-maker maker:id))
             #:attr kernel-maker (if (attribute maker) #'(quote-syntax maker) #'#f)))

  (syntax-parse stx
    [(_ (nm:id par:id) ([fld:id (~datum :) ty] ...) (par-ty ...) opts:options)
     #'(tc/builtin-struct #'nm #'par
                          (list #'fld ...)
                          (list ty ...)
                          opts.kernel-maker)]
    [(_ nm:id ([fld:id (~datum :) ty] ...) opts:options)
     #'(tc/builtin-struct #'nm #f
                          (list #'fld ...)
                          (list ty ...)
                          opts.kernel-maker)]))

(define/cond-contract (tc/make-struct-type-property prop-name pred-id struct-property-ty)
  (syntax? syntax? Type? . c:-> . tc-results/c)
  (match struct-property-ty
    [(Struct-Property: ty #f)
     (set-struct-property-pred! struct-property-ty pred-id)
     (define has-spty (make-Has-Struct-Property prop-name))
     (values->tc-results (make-Values (list (-result struct-property-ty -true-propset -empty-obj)
                                            (-result (make-pred-ty has-spty))
                                            (-result (make-Fun (list (-Arrow (list has-spty) ty
                                                                             #:props (-PS (-is-type 0 -Self)
                                                                                          -tt)))))))
                                             '())]
    [(Struct-Property: ty _) (tc-error "the predicate for ~a should be set manually. use #f instead " (syntax-e prop-name))]
    [_ (tc-error "expected ~a to be a struct type property, got ~a" (syntax-e prop-name) struct-property-ty)]))
