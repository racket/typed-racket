#lang racket/base

(require (rename-in "../utils/utils.rkt" [infer r:infer])
         racket/syntax syntax/parse syntax/stx syntax/id-table
         racket/list racket/dict racket/match racket/sequence
         (prefix-in c: (contract-req))
         (rep type-rep)
         (types utils abbrev type-table struct-table)
         (private parse-type type-annotation syntax-properties type-contract)
         (env global-env init-envs type-name-env type-alias-env
              lexical-env env-req mvar-env scoped-tvar-env
              type-alias-helper signature-env signature-helper)
         (utils tc-utils redirect-contract)
         "provide-handling.rkt" "def-binding.rkt" "tc-structs.rkt"
         "typechecker.rkt" "internal-forms.rkt"
         (typecheck provide-handling def-binding tc-structs
                    typechecker internal-forms 
                    check-below)
         syntax/location
         racket/format
         (for-template
          (only-in syntax/location quote-module-name)
          racket/base
          racket/contract/private/provide
          (env env-req)))

(provide/cond-contract
 [tc-module (syntax? . c:-> . (values syntax? syntax?))]
 [tc-toplevel-form (syntax? . c:-> . c:any/c)])

(define-logger online-check-syntax)

(define unann-defs (make-free-id-table))

(define (parse-typed-struct form)
  (parameterize ([current-orig-stx form])
    (syntax-parse form
      [t:typed-struct
       (tc/struct (attribute t.tvars) #'t.nm (syntax->list #'(t.fields ...)) (syntax->list #'(t.types ...))
                  #:mutable (attribute t.mutable)
                  #:maker (attribute t.maker)
                  #:type-only (attribute t.type-only)
                  #:prefab? (attribute t.prefab))]
      [t:typed-struct/exec
       (tc/struct null #'t.nm (syntax->list #'(t.fields ...)) (syntax->list #'(t.types ...))
                  #:proc-ty #'t.proc-type)])))

(define (type-vars-of-struct form)
  (syntax-parse form
    [t:typed-struct (attribute t.tvars)]
    [t:typed-struct/exec null]))

;; syntax? -> (listof def-binding?)
(define (tc-toplevel/pass1 form)
  (parameterize ([current-orig-stx form])
    (syntax-parse form
      #:literals (values define-values #%plain-app begin define-syntaxes)

      ;; forms that are handled in other ways
      [(~or _:ignore^ _:ignore-some^)
       (list)]

      [((~literal module) n:id spec ((~literal #%plain-module-begin) body ...))
       (list)]
       ;; module* is not expanded, so it doesn't have a `#%plain-module-begin`
      [((~literal module*) n:id spec body ...)
       (list)]

      ;; type aliases have already been handled by an earlier pass
      [_:type-alias
       (list)]

      ;; define-new-subtype
      [form:new-subtype-def
       (handle-define-new-subtype/pass1 #'form)]

      ;; declare-refinement
      ;; FIXME - this sucks and should die
      [t:type-refinement
       (match (lookup-type/lexical #'t.predicate)
              [(and t (Function: (list (arr: (list dom) (Values: (list (Result: rng _ _))) #f #f '()))))
               (let ([new-t (make-pred-ty (list dom)
                                          rng
                                          (make-Refinement dom #'t.predicate))])
                 (register-type #'t.predicate new-t))
               (list)]
              [t (tc-error "cannot declare refinement for non-predicate ~a" t)])]

      ;; require/typed
      [r:typed-require
       (let ([t (parse-type #'r.type)])
         (register-type #'r.name t)
         (list (make-def-binding #'r.name t)))]

      [r:typed-require/struct
       (let* ([t (parse-type #'r.type)]
              [flds (map fld-t (Struct-flds (lookup-type-name (Name-id t))))]
              [mk-ty (flds #f . ->* . t)])
         (register-type #'r.name mk-ty)
         (list (make-def-binding #'r.name mk-ty)))]

      ;; define-typed-struct (handled earlier)
      [(~or _:typed-struct _:typed-struct/exec)
       (list)]

      ;; predicate assertion - needed for define-type b/c or doesn't work
      [p:predicate-assertion
       (register-type #'p.predicate (make-pred-ty (parse-type #'p.type)))
       (list)]

      ;; top-level type annotation
      [t:type-declaration
       (register-type/undefined #'t.id (parse-type #'t.type))
       (register-scoped-tvars #'t.id (parse-literal-alls #'t.type))
       (list)]

      ;; definitions lifted from contracts should be ignored
      [(define-values (lifted) expr)
       #:when (contract-lifted-property #'expr)
       (list)]
      
      ;; register types of variables defined by define-values/invoke-unit forms
      [dviu:typed-define-values/invoke-unit
       (for ([export-sig (in-list (syntax->list #'(dviu.export.sig ...)))]
             [export-ids (in-list (syntax->list #'(dviu.export.members ...)))])
         (for ([id (in-list (syntax->list  export-ids))]
               [ty (in-list (map cdr (signature->bindings export-sig)))])
           (register-type-if-undefined id ty)))
       (list)]

      ;; values definitions
      [(define-values (var ...) expr)
       (define vars (syntax->list #'(var ...)))
       (syntax-parse vars
         ;; if all the variables have types, we stick them into the environment
         [(v:type-label^ ...)
          (let ([ts (map (λ (x) (get-type x #:infer #f)) vars)])
            (for-each register-type-if-undefined vars ts)
            (map make-def-binding vars ts))]
         ;; if this already had an annotation, we just construct the binding reps
         [(v:typed-id^ ...)
          (define top-level? (eq? (syntax-local-context) 'top-level))
          (for ([var (in-list vars)])
            (when (dict-has-key? unann-defs var)
              (free-id-table-remove! unann-defs var))
            (finish-register-type var top-level?))
          (stx-map make-def-binding #'(v ...) (attribute v.type))]
         ;; defer to pass1.5
         [_ (list)])]

      ;; to handle the top-level, we have to recur into begins
      [(begin . rest)
       (apply append (stx-map tc-toplevel/pass1 #'rest))]

      ;; define-syntaxes just get noted
      [(define-syntaxes (var:id ...) . rest)
       (stx-map make-def-stx-binding #'(var ...))]

      ;; otherwise, do nothing in this pass
      ;; handles expressions, provides, requires, etc and whatnot
      [_ (list)])))

;; tc-toplevel/pass1.5 : syntax? -> (listof def-binding?)
;; Handles `define-values` that still need types synthesized. Runs after
;; pass1 but before pass2.
;;
;; Note: this pass does an extra traversal of the toplevel forms which
;;       could be optimized by constructing a worklist in pass1 and only
;;       looking at forms in that list. (if performance becomes an
;;       issue with this pass we can do that)
(define (tc-toplevel/pass1.5 form)
  (parameterize ([current-orig-stx form])
    (syntax-parse form
      #:literals (define-values begin)
      [(~or _:ignore^ _:ignore-some^) (list)]

      ;; definitions lifted from contracts should be ignored
      [(define-values (lifted) expr)
       #:when (contract-lifted-property #'expr)
       (list)]

      [(define-values (var ...) expr)
       (define vars (syntax->list #'(var ...)))
       (syntax-parse vars
         ;; Do nothing for annotated/typed things
         [(v:type-label^ ...) (list)]
         [(v:typed-id^ ...) (list)]
         ;; Special case to infer types for top level defines
         ;;
         ;; Checking these will never return errors due to missing
         ;; types for annotated variables due to pass1. Checking may
         ;; error due to un-annotated variables that come later in
         ;; the module (hence we haven't synthesized a type for yet).
         [_
          (match (get-type/infer vars #'expr tc-expr tc-expr/check)
            [(list (tc-result: ts) ...)
             (for/list ([i (in-list vars)] [t (in-list ts)])
               (register-type i t)
               (free-id-table-set! unann-defs i #t)
               (make-def-binding i t))])])]

      ;; for the top-level, as for pass1
      [(begin . rest)
       (apply append (stx-map tc-toplevel/pass1.5 #'rest))]

      [_ (list)])))

;; typecheck the expressions of a module-top-level form
;; no side-effects
;; syntax? -> (or/c 'no-type tc-results/c)
(define (tc-toplevel/pass2 form [expected (tc-any-results -top)])
  
  (do-time (format "pass2 ~a line ~a"
                   (if #t
                       (substring (~a (syntax-source form))
                                  (max 0 (- (string-length (~a (syntax-source form))) 20)))
                       (syntax-source form))
                   (syntax-line form)))
  (parameterize ([current-orig-stx form])
    (syntax-parse form
      #:literal-sets (kernel-literals)
      ;; need to special case this to avoid errors at top-level
      [(~or stx:tr:class^
            stx:tr:unit^
            stx:tr:unit:invoke^
            stx:tr:unit:compound^
            stx:tr:unit:from-context^)
       (tc-expr #'stx)]
      ;; Handle define-values/invoke-unit form typechecking, by making sure that
      ;; inferred imports have the correct types
      [dviu:typed-define-values/invoke-unit
       (for ([import-sig (in-list (syntax->list #'(dviu.import.sig ...)))]
             [import-ids (in-list (syntax->list #'(dviu.import.members ...)))])
         (for ([member (in-list (syntax->list  import-ids))]
               [expected-type (in-list (map cdr (signature->bindings import-sig)))])
           (define lexical-type (lookup-type/lexical member))
           (check-below lexical-type expected-type)))
       'no-type]
      ;; these forms we have been instructed to ignore
      [stx:ignore^
       'no-type]

      ;; this is a form that we mostly ignore, but we check some interior parts
      [stx:ignore-some^
       (register-ignored! form)
       (check-subforms/ignore form)]

      ;; these forms should always be ignored
      [((~or define-syntaxes begin-for-syntax #%require #%provide #%declare) . _) 'no-type]

      ;; submodules take care of themselves:
      [(module n spec (#%plain-module-begin body ...)) 'no-type]
      ;; module* is not expanded, so it doesn't have a `#%plain-module-begin`
      [(module* n spec body ...) 'no-type]

      ;; definitions lifted from contracts should be ignored
      [(define-values (lifted) expr)
       #:when (contract-lifted-property #'expr)
       'no-type]

      ;; definitions just need to typecheck their bodies
      [(define-values () expr)
       (tc-expr/check #'expr (ret empty))
       'no-type]
      [(define-values (var ...) expr)
       #:when (for/and ([v (in-syntax #'(var ...))])
                (free-id-table-ref unann-defs v (lambda _ #f)))
       'no-type]
      [(define-values (var:typed-id^ ...) expr)
       (let ([ts (attribute var.type)])
         (when (= 1 (length ts))
           (add-scoped-tvars #'expr (lookup-scoped-tvars (stx-car #'(var ...)))))
         (tc-expr/check #'expr (ret ts)))
       'no-type]

      ;; to handle the top-level, we have to recur into begins
      [(begin) 'no-type]
      [(begin . rest)
       (for/last ([form (in-syntax #'rest)])
         (tc-toplevel/pass2 form expected))]

      ;; otherwise, the form was just an expression
      [_ (if expected
             (tc-expr/check form expected)
             (tc-expr form))])))



;; new implementation of type-check
(define (parse-def x)
  (syntax-parse x
    #:literal-sets (kernel-literals)
    [(define-values (nm ...) . rest) (syntax->list #'(nm ...))]
    [_ #f]))

(define (parse-syntax-def x)
  (syntax-parse x
    #:literal-sets (kernel-literals)
    [(define-syntaxes (nm ...) . rest) (syntax->list #'(nm ...))]
    [_ #f]))

(define-syntax-class unknown-provide-form
  (pattern
   (~and name
         (~or (~datum protect) (~datum for-syntax) (~datum for-label) (~datum for-meta)
              (~datum struct) (~datum all-from) (~datum all-from-except)
              (~datum all-defined) (~datum all-defined-except)
              (~datum prefix-all-defined) (~datum prefix-all-defined-except)
              (~datum expand)))))

;; actually do the work on a module
;; produces prelude and post-lude syntax objects
;; syntax-list -> (values syntax syntax)
(define (type-check forms0)
  (define forms (syntax->list forms0))
  (do-time "before form splitting")
  (define-values (type-aliases struct-defs stx-defs0 val-defs0 provs signature-defs)
    (filter-multiple
     forms
     type-alias? 
     (lambda (e) (or (typed-struct? e) (typed-struct/exec? e)))
     parse-syntax-def
     parse-def
     provide?
     typed-define-signature?))
  (do-time "Form splitting done")

  ;; Register signatures in the signature environment
  ;; but defer type parsing to allow mutually recursive refernces
  ;; between signatures and type aliases
  (for ([sig-form signature-defs])
    (parse-and-register-signature! sig-form))

  (define-values (type-alias-names type-alias-map)
    (get-type-alias-info type-aliases))

  ;; Add the struct names to the type table, but not with a type
  (let ((names (map name-of-struct struct-defs))
        (type-vars (map type-vars-of-struct struct-defs)))
    (for ([name names])
      (register-resolved-type-alias
       name (make-Name name 0 #t)))
    (for-each register-type-name names)
    (for-each add-constant-variance! names type-vars))
  (do-time "after adding type names")

  (register-all-type-aliases type-alias-names type-alias-map)
  (finalize-signatures!)

  (do-time "starting struct handling")
  ;; Parse and register the structure types
  (define parsed-structs
    (for/list ((def (in-list struct-defs)))
      (define parsed (parse-typed-struct def))
      (register-parsed-struct-sty! parsed)
      parsed))

  (refine-struct-variance! parsed-structs)

  ;; register the bindings of the structs
  (define struct-bindings (map register-parsed-struct-bindings! parsed-structs))
  (do-time "before pass1")
  ;; do pass 1, and collect the defintions
  (define *defs (apply append
                       (append
                        struct-bindings
                        (map tc-toplevel/pass1 forms))))
  ;; do pass 1.5 to finish up the definitions
  (define defs (append *defs (apply append (map tc-toplevel/pass1.5 forms))))
  (do-time "Finished pass1")
  ;; separate the definitions into structures we'll handle for provides
  ;; def-tbl : hash[id, binding]
  ;;    the id is the name defined by the binding
  ;; XXX: why is it ever possible that we get duplicates here?
  ;;      iow, why isn't `merge-def-binding` always `error`?
  (define def-tbl
    (for/fold ([h (make-immutable-free-id-table)])
      ([def (in-list defs)])
      ;; TODO figure out why without these checks some tests break
      (define (plain-stx-binding? def)
        (and (def-stx-binding? def) (not (def-struct-stx-binding? def))))
      (define (merge-def-bindings other-def)
        (cond
          [(not other-def) def]
          [(plain-stx-binding? def) other-def]
          [(plain-stx-binding? other-def) def]
          [else (int-err "Two conflicting definitions: ~a ~a" def other-def)]))
      (dict-update h (binding-name def) merge-def-bindings #f)))
  (do-time "computed def-tbl")
  ;; typecheck the expressions and the rhss of defintions
  ;(displayln "Starting pass2")
  (for-each tc-toplevel/pass2 forms)
  (do-time "Finished pass2")
  ;; check that declarations correspond to definitions
  (check-all-registered-types)
  ;; log messages to check-syntax to show extra types / arrows before failures
  (log-message online-check-syntax-logger
               'info
               "TR's tooltip syntaxes; this message is ignored"
               (list (syntax-property #'(void) 'mouse-over-tooltips (type-table->tooltips))))
  ;; report delayed errors
  (report-all-errors)
  ;; provide-tbl : hash[id, listof[id]]
  ;; maps internal names to all the names they're provided as
  ;; XXX: should the external names be symbols instead of identifiers?
  (define provide-tbl
    (for/fold ([h (make-immutable-free-id-table)]) ([p (in-list provs)])
      (syntax-parse p #:literal-sets (kernel-literals)
        [(#%provide form ...)
         (for/fold ([h h]) ([f (in-syntax #'(form ...))])
           (let loop ([f f])
             (syntax-parse f
               [i:id
                (dict-update h #'i (lambda (tail) (cons #'i tail)) '())]
               [((~datum rename) in out)
                (dict-update h #'in (lambda (tail) (cons #'out tail)) '())]
               [((~datum for-meta) 0 fm)
                (loop #'fm)]
               ;; is this safe?
               [((~datum for-meta) _ fm)
                h]
               [(name:unknown-provide-form . _)
                (parameterize ([current-orig-stx f])
                  (tc-error "provide: ~a not supported by Typed Racket" (syntax-e #'name.name)))]
               [_ (parameterize ([current-orig-stx f])
                    (int-err "unknown provide form"))])))]
        [_ (int-err "non-provide form! ~a" (syntax->datum p))])))
  ;; compute the new provides
  (define-values (new-stx/pre new-stx/post)
    (with-syntax*
        ([the-variable-reference (generate-temporary #'blame)]
         [mk-redirect (generate-temporary #'make-redirect)])
      (define-values (defs export-defs provs aliasess)
        (generate-prov def-tbl provide-tbl #'the-variable-reference #'mk-redirect))
      (define aliases (apply append aliasess))
      (define/with-syntax (new-defs ...) defs)
      (define/with-syntax (new-export-defs ...) export-defs)
      (define/with-syntax (new-provs ...) provs)
      (values
       #`(begin
           ;; This syntax-time submodule records all the types for all
           ;; definitions in this module, as well as type alias
           ;; definitions, structure defintions, variance information,
           ;; etc. It is `dynamic-require`d by the typechecker for any
           ;; typed module that (transitively) depends on this
           ;; module. We keep this in a submodule and use
           ;; `dynamic-require` so that we don't load any of this code
           ;; (and in particular all of its dependencies on the type
           ;; checker) when just running a typed module.
           (begin-for-syntax
             (module* #%type-decl #f
               (#%plain-module-begin ;; avoid top-level printing and config
                (#%declare #:empty-namespace) ;; avoid binding info from here
                (require typed-racket/types/numeric-tower typed-racket/env/type-name-env
                         typed-racket/env/global-env typed-racket/env/type-alias-env
                         typed-racket/types/struct-table typed-racket/types/abbrev
                         (rename-in racket/private/sort [sort raw-sort]))
                #,(env-init-code)
                #,(talias-env-init-code)
                #,(tname-env-init-code)
                #,(tvariance-env-init-code)
                #,(mvar-env-init-code mvar-env)
                #,(signature-env-init-code)
                #,(make-struct-table-code)
                #,@(for/list ([a (in-list aliases)])
                     (match-define (list from to) a)
                     #`(add-alias (quote-syntax #,from) (quote-syntax #,to))))))
           (begin-for-syntax (add-mod! (variable-reference->module-path-index
                                        (#%variable-reference))))

           ;; FIXME: share this variable reference with the one below
           (define the-variable-reference (quote-module-name))
           ;; Here we construct the redirector for the #%contract-defs
           ;; submodule. The `mk-redirect` identifier is also used in
           ;; the `new-export-defs`.
           (begin-for-syntax
            ;; We explicitly insert a `require` here since this module
            ;; is `lazy-require`d and thus just doing a `require`
            ;; outside wouldn't actually make the module
            ;; available. The alternative would be to add an
            ;; appropriate-phase `require` statically in a module
            ;; that's non-dynamically depended on by
            ;; `typed/racket`. That makes for confusing non-local
            ;; dependencies, though, so we do it here.
            (require typed-racket/utils/redirect-contract)
            ;; We need a submodule for a for-syntax use of
            ;; `define-runtime-module-path`:
            (module #%contract-defs-reference racket/base
              (require racket/runtime-path
                       (for-syntax racket/base))
              (define-runtime-module-path-index contract-defs-submod
                '(submod ".." #%contract-defs))
              (provide contract-defs-submod))
            (require (submod "." #%contract-defs-reference))
            ;; Create the redirection funtion using a reference to
            ;; the submodule that is friendly to `raco exe`:
            (define mk-redirect
              (make-make-redirect-to-contract contract-defs-submod)))

           ;; This submodule contains all the definitions of
           ;; contracted identifiers. For an exported definition like
           ;;    (define f : T e)
           ;; we generate the following defintions that go in the
           ;; submodule:
           ;;    (define con ,(type->contract T))
           ;;    (define f* (contract con 'pos 'neg f))
           ;;    (provide (rename-out [f* f]))
           ;; This is the `new-defs ...`.
           ;; The `extra-requires` which go in the submodule are used
           ;; (potentially) in the implementation of the contracts.
           ;;
           ;; The reason to construct this submodule is to avoid
           ;; loading the contracts (or the `racket/contract` library
           ;; itself) at the runtime of typed modules that don't need
           ;; them. This is similar to the reason for the
           ;; `#%type-decl` submodule. 
           (module* #%contract-defs #f
             (#%plain-module-begin
              (#%declare #:empty-namespace) ;; avoid binding info from here
              #,extra-requires
              new-defs ...)))
       #`(begin
           ;; Now we create definitions that are actually provided
           ;; from the module itself. There are two levels of
           ;; indirection here (see the implementation in
           ;; provide-handling.rkt).
           ;;
           ;; First, we generate a macro that expands to a
           ;; `local-require` of the contracted identifier in the
           ;; #%contract-defs submodule:
           ;;    (define-syntax con-f (mk-redirect f))
           ;;
           ;; Then, we define a macro that is a rename-transformer for
           ;; either the original `f` or `con-f`.
           ;;    (define-syntax export-f (renamer f con-f))
           ;;
           ;; Note that we can't combine any of these indirections,
           ;; because it's important for `export-f` to be a
           ;; rename-transformer (making things like
           ;; `syntax-local-value` work right), but `con-f` can't be,
           ;; since it expands to a `local-require`.
           new-export-defs ...

           ;; Finally, we do the export:
           ;;    (provide (rename-out [export-f f]))
           new-provs ...))))
  (do-time "finished provide generation")
  (values new-stx/pre new-stx/post))

;; typecheck a whole module
;; syntax -> (values syntax syntax)
(define (tc-module stx)
  (syntax-parse stx
    [(pmb . forms) (begin0 (type-check #'forms) (do-time "finished type checking"))]))

;; typecheck a top-level form that does not have any
;; top-level `begin`s
;; used only from #%top-interaction
;; syntax -> (or/c 'no-type tc-results/c)
(define (tc-toplevel-form form)
  ;; Handle type aliases
  (when (type-alias? form)
    (define-values (alias-names alias-map)
      (get-type-alias-info (list form)))
    (register-all-type-aliases alias-names alias-map))
  ;; Handle struct definitions
  (when (typed-struct? form)
    (define name (name-of-struct form))
    (define tvars (type-vars-of-struct form))
    (register-type-name name)
    (add-constant-variance! name tvars)
    (define parsed (parse-typed-struct form))
    (register-parsed-struct-sty! parsed)
    (refine-struct-variance! (list parsed))
    (register-parsed-struct-bindings! parsed))
  (tc-toplevel/pass1 form)
  (tc-toplevel/pass1.5 form)
  (begin0 (tc-toplevel/pass2 form #f)
          (report-all-errors)))

;; handle-define-new-subtype/pass1 : Syntax -> Empty
(define (handle-define-new-subtype/pass1 form)
  (syntax-parse form
    [form:new-subtype-def
     ;; (define-new-subtype-internal name (constructor rep-type) #:gen-id gen-id)
     (define ty (parse-type (attribute form.name)))
     (define rep-ty (parse-type (attribute form.rep-type)))
     (register-type (attribute form.constructor) (-> rep-ty ty))
     (list)]))

