#lang racket/base

(require "../utils/utils.rkt"
         syntax/struct syntax/parse racket/function racket/match racket/list syntax/id-set
         syntax/id-table
         (prefix-in c: (contract-req))
         "../rep/type-rep.rkt"
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
;; proc-ty: (Option Type)
(struct struct-desc (parent-fields self-fields tvars
                     mutable parent-mutable proc-ty)
        #:transparent)

(define (struct-desc-all-fields fields)
  (append (struct-desc-parent-fields fields) (struct-desc-self-fields fields)))
(define (struct-desc-parent-count fields)
  (length (struct-desc-parent-fields fields)))

(define (name-of-struct stx)
  (syntax-parse stx
    [(~or t:typed-struct t:typed-struct/exec)
     #'t.type-name]))

(define-syntax-class expanded-props
  #:literals (null list #%plain-app)
  (pattern null
           #:with (prop-names ...) #'()
           #:with (prop-vals ...) #'())
  (pattern (#%plain-app list (#%plain-app cons prop-names prop-vals) ...)))


(define (tc/struct-prop-values form name)
  (syntax-parse form
    #:literals (define-values #%plain-app define-syntaxes begin #%expression let-values quote list cons make-struct-type values null)
    [(define-values (struct-var r ...)
       (let-values (((var1 r1 ...)
                     (let-values ()
                       (#%expression
                        (let-values ()
                          (#%plain-app make-struct-type _name _super-type _init-fcnt _auto-fcnt auto-v props:expanded-props r_args ...))))))
         (#%plain-app values args-v ...)))
     (let ([pnames (attribute props.prop-names)])
       (unless (null? pnames)
         (define sty (lookup-type-name name))
         (for/list ([p (in-list pnames)]
                    [pval (in-list (attribute props.prop-vals))])
           (match (single-value p)
             [(tc-result1: (Struct-Property: ty _))
              (match-define (F: var) -Self)
              (match-define (F: var-imp) -Imp)
              (match sty
                [(Struct: _ _ _ _ _ _ _)
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
              (tc-error "expected a struct type property but got ~a" ty)]))))]

    [(define-syntaxes (nm ...) . rest) (void)]))



;; parse name field of struct, determining whether a parent struct was specified
(define/cond-contract (parse-parent nm/par prefab?)
  (c:-> syntax? c:any/c (values identifier? (c:or/c Name? #f) (c:or/c Mu? Poly? Struct? Prefab? #f)))
  (syntax-parse nm/par
    [v:parent
     (if (attribute v.par)
         (let* ([parent0 (parse-type
                          (cond
                            ;; maybe-struct-info-wrapper-type only returns
                            ;; parent's type name when the parent structure
                            ;; defined either is in a typed module or
                            ;; imported via require/typed
                            [(lookup-struct-name #'v.par)]
                            [(maybe-struct-info-wrapper-type (syntax-local-value #'v.par (lambda () #f)))]
                            ;; if the parent struct is a builtin structure like exn,
                            ;; its structure name is also its type name.
                            [else #'v.par]))]
                [parent (let loop ((parent parent0))
                          (cond
                            ((Name? parent) (loop (resolve-name parent)))
                            ((or (Poly? parent) (Mu? parent)
                                 (if prefab? (Prefab? parent) (Struct? parent)))
                             parent)
                            (else
                             (tc-error/stx #'v.par "parent type not a valid structure name: ~a"
                                           (syntax->datum #'v.par)))))])
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
                 (struct-desc-proc-ty desc)
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
  (register-type-name type-name
                      (make-Poly (struct-desc-tvars desc) sty)))

;; Register the appropriate types, return a list of struct bindings
(define/cond-contract (register-struct-bindings! sty names desc si)
  (c:-> (c:or/c Struct? Prefab?) struct-names? struct-desc? (c:or/c #f struct-info?) (c:listof binding?))
  (match sty
    [(? Struct?) (register-non-prefab-bindings! sty names desc si)]
    [(? Prefab?) (register-prefab-bindings! sty names desc si)]))

;; a helper predicate function for values created by mk-type-alias
(define (type-alias? a)
  (or (Name? a) (App? a)))

;; Generates an alias of the (struct) type that the `type-name` references.  The
;; alias is mainly used to help create the types of the constructor, field
;; accessors and mutators etc.  If the original (struct) type is monomorphic,
;; i.e. `tvars` is null, the alias is a `Name`. Otherwise, it is an `App`
;; to encode (StructTypeName tvar ...)
(define/cond-contract (mk-type-alias type-name tvars)
  (c:-> identifier? (c:listof symbol?) type-alias?)
  (define name-type (make-Name type-name (length tvars) #t))
  (if (null? tvars)
      name-type
      (make-App name-type (map make-F tvars))))

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
  (match-define (struct-desc parent-fields self-fields tvars mutable parent-mutable _) desc)

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
  (define st-type-alias (mk-type-alias type-name tvars))

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
                   #:proc-ty [proc-ty #f]
                   #:maker [maker #f]
                   #:extra-maker [extra-maker #f]
                   #:mutable [mutable #f]
                   #:type-only [type-only #f]
                   #:prefab? [prefab? #f]
                   #:properties [properties empty])
  (define-values (nm parent-name parent) (parse-parent nm/par prefab?))
  ;; create type variables for the new type parameters
  (define tvars (map syntax-e vars))
  (define new-tvars (map make-F tvars))
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
           (struct-desc parent-fields types tvars mutable parent-mutable #f))
         (parsed-struct (make-Prefab key (append parent-fields types))
                        names desc (struct-info-property nm/par) #f)]
        [else
         (define maybe-proc-ty
           (let ([maybe-parsed-proc-ty (and proc-ty (parse-type proc-ty))])
             (and maybe-parsed-proc-ty
                  (cond
                    ;; ensure that the prop:procedure argument is really a procedure
                    [(subtype maybe-parsed-proc-ty top-func)
                     maybe-parsed-proc-ty]
                    [else (expected-but-got top-func maybe-parsed-proc-ty)
                          #f]))))

         (define parent-mutable
           ;; Only valid as long as typed structs must be
           ;; either fully mutable or fully immutable
           (or (not parent)
               (andmap fld-mutable? (get-flds concrete-parent))))

         (define desc (struct-desc (map fld-t (get-flds concrete-parent))
                                   types tvars
                                   mutable parent-mutable maybe-proc-ty))

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
  (define desc (struct-desc parent-tys tys null #f #f #f))
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
