#lang racket/base

(require "../utils/utils.rkt"
         syntax/struct syntax/parse racket/function racket/match racket/list

         (prefix-in c: (contract-req))
         (rep type-rep object-rep free-variance)
         (private parse-type syntax-properties)
         (types abbrev subtype utils resolve substitute struct-table prefab)
         (env global-env type-name-env type-alias-env tvar-env)
         (utils tc-utils)
         (typecheck def-binding internal-forms error-message)
         (for-syntax syntax/parse racket/base))

(require-for-cond-contract racket/struct-info)

(provide tc/struct
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


;; parse name field of struct, determining whether a parent struct was specified
(define/cond-contract (parse-parent nm/par prefab?)
  (c:-> syntax? c:any/c (values identifier? (c:or/c Name? #f) (c:or/c Mu? Poly? Struct? Prefab? #f)))
  (syntax-parse nm/par
    [v:parent
      (if (attribute v.par)
          (let* ([parent0 (parse-type #'v.par)]
                 ;; TODO ensure this is a struct
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
    [(Struct: _ _ flds _ _ _) flds]
    [#f null]))


;; Constructs the Struct value for a structure type
;; The returned value has free type variables
(define/cond-contract (mk/inner-struct-type names desc parent)
  (c:-> struct-names? struct-desc? (c:or/c Struct? #f) Struct?)

  (let* ([this-flds (for/list ([t (in-list (struct-desc-self-fields desc))]
                               [g (in-list (struct-names-getters names))])
                       (make-fld t g (struct-desc-mutable desc)))]
         [flds (append (get-flds parent) this-flds)])
    (make-Struct (struct-names-struct-name names)
                 parent flds (struct-desc-proc-ty desc)
                 (not (null? (struct-desc-tvars desc)))
                 (struct-names-predicate names))))


;; construct all the various types for structs, and then register the approriate names
;; identifier listof[identifier] type listof[fld] listof[Type] boolean ->
;;  (values Type listof[Type] listof[Type])
(define/cond-contract (register-sty! sty names desc)
  (c:-> (c:or/c Struct? Prefab?) struct-names? struct-desc? void?)

  ;; a type alias needs to be registered here too, to ensure
  ;; that parse-type will map the identifier to this Name type
  (define type-name (struct-names-type-name names))
  (register-resolved-type-alias
   type-name
   (make-Name type-name (length (struct-desc-tvars desc)) (Struct? sty)))
  (register-type-name type-name
                      (make-Poly (struct-desc-tvars desc) sty)))




;; Register the approriate types to the struct bindings.
(define/cond-contract (register-struct-bindings! sty names desc si)
  (c:-> (c:or/c Struct? Prefab?) struct-names? struct-desc? (c:or/c #f struct-info?) (c:listof binding?))


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
                                         ;; FIXME: does this make sense with prefabs?
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
          (add-struct-fn! g path #f)
          (make-def-binding g func)))
      (if mutable
          (for/list ([s (in-list (struct-names-setters names))]
                     [t (in-list self-fields)]
                     [i (in-naturals parent-count)])
            (add-struct-fn! s (make-StructPE poly-base i) #t)
            (make-def-binding s (poly-wrapper (->* (list poly-base t) -Void))))
          null))))

  (define extra-constructor (struct-names-extra-constructor names))

  (define constructor-binding
    (make-def-binding (struct-names-constructor names)
                      (poly-wrapper (->* all-fields poly-base))))
  (define constructor-bindings
    (cons constructor-binding
          (if extra-constructor
              (list (make-def-binding extra-constructor
                                      (poly-wrapper (->* all-fields poly-base))))
              null)))

  (for ([b (append constructor-bindings bindings)])
    (register-type (binding-name b) (def-binding-ty b)))

  (append
    (if (free-identifier=? (struct-names-type-name names)
                           (struct-names-constructor names))
      null
      (list constructor-binding))
   (cons
     (make-def-struct-stx-binding (struct-names-type-name names) si (def-binding-ty constructor-binding))
     bindings)))

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
                   #:prefab? [prefab? #f])
  ;; parent field types can't actually be determined here
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
             ;; no parent, so trivially true
             ['() #t]))
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
         (define sty (mk/inner-struct-type names desc concrete-parent))
         
         (parsed-struct sty names desc (struct-info-property nm/par) type-only)]))

;; register a struct type
;; convenience function for built-in structs
;; tc/builtin-struct : identifier Maybe[identifier] Listof[identifier]
;;                     Listof[Type] Maybe[identifier] Listof[Type]
;;                     -> void
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
  (register-struct-bindings! sty names desc #f)
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

