#lang racket/base

(require "../utils/utils.rkt"
         syntax/struct syntax/parse racket/function racket/match racket/list syntax/id-set
         syntax/id-table
         (prefix-in c: (contract-req))
         
         (rep type-rep free-variance values-rep)
         (private parse-type syntax-properties)
         (types base-abbrev abbrev subtype utils resolve substitute struct-table)
         (env global-env type-name-env type-alias-env tvar-env lexical-env struct-name-env)
         (utils tc-utils prefab identifier)
         (only-in (utils struct-info) maybe-struct-info-wrapper-type)
         (typecheck typechecker def-binding internal-forms error-message tc-subst renamer)
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

(define/cond-contract (register-prefab-bindings! pty names desc si)
  (c:-> Prefab? struct-names? struct-desc? (c:or/c #f struct-info?) (c:listof binding?))
  (define key (Prefab-key pty))
  (match-define
    (struct-desc parent-fields self-fields
                 constructor-tvars
                 self-mutable parent-mutable _)
    desc)
  (define any-mutable (or self-mutable parent-mutable))
  (define all-fields (append parent-fields self-fields))
  (define self-count (length self-fields))
  (define parent-count (length parent-fields))
  (define field-count (+ self-count parent-count))
  (define field-univs (build-list field-count (λ (_) Univ)))
  (define field-tvar-syms
    (build-list field-count (λ (_) (gen-pretty-sym))))
  (define field-tvar-Fs (map make-F field-tvar-syms))
  (define raw-poly-prefab ;; since all prefabs are polymorphic by nature
    (make-Prefab key field-tvar-Fs))

  (define prefab-top-type (make-PrefabTop key))

  (define bindings
    (list*
     ;; the list of names w/ types
     (make-def-binding (struct-names-struct-type names) (make-StructType pty))
     (make-def-binding (struct-names-predicate names)
                       (make-pred-ty prefab-top-type))
     (append
      (for/list ([acc-id (in-list (struct-names-getters names))]
                 [t (in-list self-fields)]
                 [idx (in-naturals parent-count)])
        (let* ([path (make-PrefabPE key idx)]
               [fld-sym (list-ref field-tvar-syms idx)]
               [fld-t (list-ref field-tvar-Fs idx)]
               [func-t (cond
                         [(or self-mutable parent-mutable)
                          ;; NOTE - if we ever track mutable fields more ganularly
                          ;; than "all of the fields are mutable or not" then this
                          ;; could be more precise (i.e. include the path elem
                          ;; for any immutable field).
                          (make-Poly
                           field-tvar-syms
                           (cl-> [(raw-poly-prefab) fld-t]
                                 [(prefab-top-type) Univ]))]
                       [else
                        (make-Poly
                         (list fld-sym)
                         (cl->*
                          (->acc (list (make-Prefab key (list-set field-univs idx fld-t)))
                                 fld-t
                                 (list path))
                          (-> prefab-top-type Univ)))])])
          (add-struct-accessor-fn! acc-id prefab-top-type idx self-mutable #t)
          (make-def-binding acc-id func-t)))
      (if self-mutable
          (for/list ([s (in-list (struct-names-setters names))]
                     [t (in-list self-fields)]
                     [idx (in-naturals parent-count)])
            (let ([fld-t (list-ref field-tvar-Fs idx)])
              (add-struct-mutator-fn! s prefab-top-type idx #t)
              (make-def-binding s (make-Poly
                                   field-tvar-syms
                                   (->* (list raw-poly-prefab fld-t) -Void)))))
          null))))

  ;; the base-type, with free type variables
  ;; NOTE: This type is only used for the constructor
  ;;       of a prefab---other operators are entirely polymorphic
  (define name-type
    (make-Name (struct-names-type-name names)
               (length constructor-tvars)
               #t))
  (define poly-base
    (if (null? constructor-tvars)
        name-type
        (make-App name-type (map make-F constructor-tvars))))
  (define extra-constructor (struct-names-extra-constructor names))

  (define constructor-binding
    (make-def-binding (struct-names-constructor names)
                      (make-Poly constructor-tvars
                                 (->* all-fields poly-base))))
  (define constructor-bindings
    (cons constructor-binding
          (if extra-constructor
              (list (make-def-binding
                     extra-constructor
                     (make-Poly constructor-tvars
                                (->* all-fields poly-base))))
              null)))

  (for ([b (in-list (append constructor-bindings bindings))])
    (register-type (binding-name b) (def-binding-ty b)))

  (append
   (if (free-identifier=? (struct-names-type-name names)
                          (struct-names-constructor names))
       null
       (list constructor-binding))
   (cons
    (make-def-struct-stx-binding (struct-names-struct-name names)
                                 (struct-names-struct-name names)
                                 (struct-names-type-name names)
                                 si
                                 (def-binding-ty constructor-binding)
                                 extra-constructor)
    bindings)))

(define/cond-contract (register-non-prefab-bindings! sty names desc si)
  (c:-> Struct? struct-names? struct-desc? (c:or/c #f struct-info?) (c:listof binding?))
  (define tvars (struct-desc-tvars desc))
  (define all-fields (struct-desc-all-fields desc))
  (define parent-fields (struct-desc-parent-fields desc))
  (define self-fields (struct-desc-self-fields desc))
  (define mutable (struct-desc-mutable desc))
  (define parent-mutable (struct-desc-parent-mutable desc))
  (define parent-count (struct-desc-parent-count desc))


  ;; the base-type, with free type variables
  (define name-type
    (make-Name (struct-names-type-name names) (length tvars) #t))
  (define poly-base
    (if (null? tvars)
        name-type
        (make-App name-type (map make-F tvars))))

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
  (define bindings
    (list*
     ;; the list of names w/ types
     (make-def-binding (struct-names-struct-type names) (make-StructType sty))
     (make-def-binding (struct-names-predicate names)
                       (make-pred-ty (if (not covariant?)
                                         (make-StructTop sty)
                                         (subst-all (make-simple-substitution
                                                     tvars (map (const Univ) tvars)) poly-base))))
     (append
      (for/list ([g (in-list (struct-names-getters names))]
                 [t (in-list self-fields)]
                 [i (in-naturals parent-count)])
        (let* ([path (make-StructPE poly-base i)]
               [func (poly-wrapper
                      (if mutable
                          (->* (list poly-base) t)
                          (->acc (list poly-base) t (list path))))])
          (add-struct-accessor-fn! g poly-base i mutable #f)
          (make-def-binding g func)))
      (if mutable
          (for/list ([s (in-list (struct-names-setters names))]
                     [t (in-list self-fields)]
                     [i (in-naturals parent-count)])
            (add-struct-mutator-fn! s poly-base i #f)
            (make-def-binding s (poly-wrapper (->* (list poly-base t) -Void))))
          null))))

  (define struct-name (struct-names-struct-name names))
  (define type-name (struct-names-type-name names))
  (define extra-constructor (struct-names-extra-constructor names))
  (define constructor-type (poly-wrapper (->* all-fields poly-base)))
  (define struct-binding (make-def-struct-stx-binding struct-name
                                                      struct-name
                                                      type-name
                                                      si
                                                      constructor-type
                                                      extra-constructor))
  (define def-bindings
    (if extra-constructor
        (cons (make-def-binding extra-constructor
                                constructor-type)
              bindings)
        bindings))

  (register-type (struct-names-constructor names) constructor-type)
  (for ([b (in-list def-bindings)])
    (register-type (binding-name b) (def-binding-ty b)))

  (cons struct-binding
        (append
         (if (free-identifier=? type-name
                                struct-name)
             null
             ;; since type-name is also an syntax transformer that contains the
             ;; struct info, we generate a struct stx binding for it here
             (list (make-def-struct-stx-binding
                    type-name
                    struct-name
                    type-name
                    si
                    constructor-type
                    extra-constructor)))
         def-bindings)))



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

         (define desc (struct-desc
                       (map fld-t (get-flds concrete-parent))
                       types
                       tvars
                       mutable
                       parent-mutable
                       maybe-proc-ty))

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
                                                                                          (-not-type 0 -Self))))))))
                                             '())]
    [(Struct-Property: ty _) (tc-error "the predicate for ~a should be set manually. use #f instead " (syntax-e prop-name))]
    [_ (tc-error "expected ~a to be a struct type property, got ~a" (syntax-e prop-name) struct-property-ty)]))
