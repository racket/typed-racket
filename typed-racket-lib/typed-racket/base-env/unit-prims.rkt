#lang racket/base

;; Primitive forms for units/signatures

(provide unit
         define-unit
         compound-unit 
         define-compound-unit
         compound-unit/infer
         define-compound-unit/infer
         invoke-unit
         invoke-unit/infer
         define-values/invoke-unit
         define-values/invoke-unit/infer
         unit-from-context)


(require  "../utils/utils.rkt"
          "colon.rkt"
          "../private/unit-literals.rkt"
          (for-syntax syntax/parse
                      racket/base
                      racket/list
                      racket/match
                      racket/syntax
                      syntax/context
                      syntax/flatten-begin
                      syntax/kerncase
                      "../private/syntax-properties.rkt"
                      (typecheck internal-forms)
                      syntax/id-table
                      racket/dict
                      racket/unit-exptime
                      syntax/strip-context
                      (utils tc-utils)
                      syntax/id-table
                      syntax/id-set)
          (prefix-in untyped- (only-in racket/unit
                                       define-signature
                                       unit
                                       invoke-unit
                                       invoke-unit/infer
                                       compound-unit
                                       define-unit
                                       define-compound-unit
                                       define-values/invoke-unit
                                       define-values/invoke-unit/infer
                                       compound-unit/infer
                                       define-compound-unit/infer
                                       unit-from-context))
          (only-in racket/unit
                   extends
                   import
                   export
                   init-depend
                   link
                   prefix
                   rename)
          "base-types.rkt"
          "base-types-extra.rkt"
          (for-label "colon.rkt")
          (for-template (rep type-rep))
          (submod "../typecheck/internal-forms.rkt" forms))

(begin-for-syntax
  (define-literal-set colon #:for-label (:))

  ;; TODO: extend for other sig forms
  (define-syntax-class def-sig-form
    #:literal-sets (colon)
    (pattern [name:id : type]
             #:with internal-form #'(name type)
             #:with erased #'name))

  (define-splicing-syntax-class extends-form
    #:literals (extends)
    (pattern (~seq extends super:id)
             #:with internal-form #'super
             #:with extends-id #'super
             #:attr form #'(extends super))
    (pattern (~seq)
             #:with internal-form #'#f
             #:with extends-id '()
             #:attr form '()))

  (define-splicing-syntax-class init-depend-form
    #:literals (init-depend)
    (pattern (init-depend sig:id ...)
             #:attr form (list #'(init-depend sig ...))
             #:with names #'(sig ...))
    (pattern (~seq)
             #:attr form '()
             #:with names #'()))
  
  (define-syntax-class unit-expr
    (pattern e
             #:with val #'e))
  
  ;; More general handling of import/export signatures in units
  (define-syntax-class unit-imports
    #:literals (import)
    (pattern (import sig:sig-spec ...)
             #:with names #'(sig.sig-name ...)
             #:attr renamers (attribute sig.rename)))

  (define-syntax-class unit-exports
    #:literals (export)
    (pattern (export sig:sig-spec ...)
             #:with names #'(sig.sig-name ...)
             #:attr renamers (attribute sig.rename)))

  ;; The `rename` attribute in the sig-spec syntax class is used to correctly
  ;; map names of signature bound variables in unit bodies to their names in
  ;; the fully expanded syntax. It applies prefixes and renamings from
  ;; signature specifications to identifiers.
  (define-syntax-class sig-spec
    #:literals (prefix rename)
    (pattern sig-id:id
             #:attr rename (lambda (id) id)
             #:with sig-name #'sig-id)
    (pattern (prefix p:id sig:sig-spec)
             #:attr rename (lambda (id) (format-id #'sig.sig-name
                                                 "~a~a"
                                                 #'p
                                                 ((attribute sig.rename) id)))
             #:with sig-name #'sig.sig-name)
    (pattern (rename sig:sig-spec (new:id old:id) ...)
             #:attr rename 
             (lambda (id)
               (define mapping  (map cons  
                                     (syntax->list #'(old ...))
                                     (syntax->list #'(new ...))))
               (define (lookup id) 
                 (define lu (member id mapping 
                                    (lambda (x y) 
                                      (free-identifier=? x (car y)))))
                 (and lu (cdar lu)))

               (define rn ((attribute sig.rename) id))
               (or (lookup rn) rn))
             #:with sig-name #'sig.sig-name))

  ;; extracts the signature members from syntax representing a sequence of signature name
  (define (signatures-vars stx)
    (define (signature-vars sig-id)
      (let-values ([(_0 vars _2 _3)
                    (signature-members sig-id sig-id)])
        vars))
    (apply append (map signature-vars (syntax->list stx)))))


;; Abstraction for creating trampolining macros
(begin-for-syntax
  (define-syntax-class (begin-form name [arg #'()])
    #:literals (begin)
    (pattern (begin e ...)
             #:with trampoline-form
             #`(#,name #,@arg e ...)))
  (define-syntax-class (name-form name)
    (pattern (_ e ...)
             #:with trampoline-form
             #`(begin (#,name e) ...)))
  (define-splicing-syntax-class (rest-form name arg)
    (pattern (~seq) 
             #:with trampoline-form
             #`(begin))
    (pattern (~seq e1 e2 ...)
             #:with trampoline-form
             #`(begin (#,name #,@arg e1)
                      (#,name #,@arg e2) ...))))

(define-syntax (define-trampolining-macro stx)
  (syntax-parse stx
    [(_ name:id case ...)
     #`(define-syntax (name stx)
         (syntax-parse stx
           [(_) #'(begin)]
           [(name e)
            (define exp-e 
              (local-expand #'e (syntax-local-context) (kernel-form-identifier-list)))
            (syntax-parse exp-e
              #:literal-sets (kernel-literals)
              [(~var b (begin-form #'name))
               #'b.trampoline-form]
              case ...
              [_ 
               exp-e])]
           [(~var e (name-form #'name))
            #'e.trampoline-form]))]
    [(_ (name:id arg ...) case ...)
     #`(define-syntax (name stx)
         (syntax-parse stx
           [(_ arg ...) #'(begin)]
           [(name arg ... e) 
            (define exp-e 
              (local-expand #'e (syntax-local-context) (kernel-form-identifier-list)))
            (syntax-parse exp-e
              #:literal-sets (kernel-literals)
              [(~var b (begin-form #'name #`(arg ...)))
               #'b.trampoline-form]
              case ...
              [_ exp-e])]
           [(_ arg ... (~var exprs (rest-form #'name #`(arg ...))))
            #'exprs.trampoline-form]))]))


;; imports/members : identifier? -> syntax?
;; given an identifier representing a signature
;; returns syntax containing the signature name and the names of each variable contained
;; in the signature, this is needed to typecheck define-values/invoke-unit forms
(define-for-syntax (imports/members sig-id)
  (match-define-values (_ (list imp-mem ...) _ _)
                       (signature-members sig-id sig-id))
  #`(#,sig-id #,@(map (lambda (id)
                        (local-expand
                         id
                         (syntax-local-context)
                         (kernel-form-identifier-list)))
                      imp-mem)))

(define-for-syntax (process-dv-exports es)
  (for/list ([e (in-list es)])
    (syntax-parse e
      [s:sig-spec
       (define sig-id #'s.sig-name)
       (define renamer (attribute s.rename))
       (match-define-values (_ (list ex-mem ...) _ _)
                            (signature-members sig-id sig-id))
       #`(#,sig-id #,@(map renamer ex-mem))])))

;; Typed macro for define-values/invoke-unit
;; This has to be handled specially because the types of
;; the defined values must be registered in the environment
(define-syntax (define-values/invoke-unit stx)
  (syntax-parse stx 
    #:literals (import export)
    [(_ unit-expr
        (import isig:sig-spec ...)
        (export esig:sig-spec ...))
     (define imports-stx (syntax->list #'(isig.sig-name ...)))
     (define exports-stx (syntax->list #'(esig ...)))
     (define/with-syntax temp (syntax-local-introduce (generate-temporary)))
     #`(begin
         #,(internal (quasisyntax/loc stx
                       (define-values/invoke-unit-internal
                         (#,@(map imports/members imports-stx))
                         (#,@(process-dv-exports exports-stx)))))
         (: temp (Unit (import isig.sig-name ...)
                       (export esig.sig-name ...)
                       (init-depend isig.sig-name ...)
                       ;; FIXME this needs to be AnyValues
                        Any))
         (define temp unit-expr)
         #,(ignore (quasisyntax/loc stx
                     (untyped-define-values/invoke-unit unit-expr
                                                        (import isig ...)
                                                        (export esig ...)))))]))
(begin-for-syntax
  (define (get-imports/exports unit-ids)
    (for/fold ([imports null]
               [exports null])
              ([unit-id (in-list unit-ids)])
      (match-define-values ((list (cons _ new-imports) ...)
                            (list (cons _ new-exports) ...))
                           (unit-static-signatures unit-id unit-id))
      (values (append imports new-imports) (append exports new-exports))))
  
  (define (infer-imports unit-ids)
    (define-values (imports exports) (get-imports/exports unit-ids))
    (free-id-set->list (free-id-set-subtract (immutable-free-id-set imports)
                                             (immutable-free-id-set exports))))

  ;; infer-exports returns all the exports from linked
  ;; units rather than just those that are not also
  ;; imported
  (define (infer-exports unit-ids)
    (define-values (imports exports) (get-imports/exports unit-ids))
    exports)
  
  (define-syntax-class define/invoke/infer-form
    #:literals (define-values/invoke-unit/infer)
    (pattern (define-values/invoke-unit/infer 
               exports:maybe-exports
               us:unit-spec)
             #:with untyped-stx
             #`(untyped-define-values/invoke-unit/infer
                #,@#'exports us)
             #:attr inferred-imports
             (infer-imports (attribute us.unit-ids))
             #:attr inferred-exports
             (or (attribute exports.exports)
                 (infer-exports (attribute us.unit-ids)))))
  
  (define-splicing-syntax-class maybe-exports
    #:literals (export)
    (pattern (~seq)
             #:attr exports #f)
    (pattern (export sig:id ...)
             #:attr exports (syntax->list #'(sig ...))))
  
  (define-syntax-class unit-spec
    #:literals (link)
    (pattern unit-id:id
             #:attr unit-ids (syntax->list #'(unit-id))
             #:attr link-units? #f
             #:with untyped-stx (tr:unit:invoke:expr-property #'(#%expression unit-id) #t)
             #:with last-id (local-expand #'unit-id
                                          (syntax-local-context)
                                          (kernel-form-identifier-list)))
    (pattern (link uid-inits:id ... uid-last:id)
             #:attr link-units? #t
             #:with untyped-stx
             #`(link uid-inits ... uid-last)
             #:attr unit-ids (syntax->list #'(uid-inits ... uid-last))
             #:with last-id (local-expand #'uid-last
                                          (syntax-local-context)
                                          (kernel-form-identifier-list)))))

;; Note: This may not correctly handle all use cases of
;; define-values/invoke-unit/infer
;; inferred imports and exports are handled in the following way
;; - the exports of ALL units being linked are added to the export list
;;   to be registered in tc-toplevel
;; - imports are the set-difference of the union of all imports and the
;;   union of all exports
;; This seems to correctly handle both recursive and non-recursive
;; linking patterns
(define-syntax (define-values/invoke-unit/infer stx)
  (syntax-parse stx
    [dviui:define/invoke/infer-form
     #`(begin
         #,(internal (quasisyntax/loc stx
                       (define-values/invoke-unit-internal
                         (#,@(map imports/members (attribute dviui.inferred-imports)))
                         (#,@(process-dv-exports
                              (attribute dviui.inferred-exports))))))
         #,(ignore (quasisyntax/loc stx dviui.untyped-stx)))]))

;; invoke-unit macro
(begin-for-syntax
  (define-splicing-syntax-class invoke-imports
    #:literals (import)
    (pattern (~seq)
             #:attr untyped-import #'()
             #:with imports #'())
    (pattern (import sig:id ...)
             #:attr untyped-import #'((import sig ...))
             #:with imports #'((quote-syntax sig) ...))))


;; need to do extra work to make this work with the existing
;; invoke-unit machinery
;; need to also add the local-expanded unit-ids to the table ..
(define-syntax (invoke-unit/infer stx)
  (syntax-parse stx 
    [(_ us:unit-spec)
     (define imports (map
                      (lambda (sig) (replace-context stx sig))
                      (infer-imports (attribute us.unit-ids))))
     (define unit-expr-id #'us.last-id)
     (ignore
      (tr:unit:invoke
       (quasisyntax/loc stx
         (#%expression
          (begin
            (void (quote-syntax #,unit-expr-id))
            (void #,@(map (lambda (id) #`(quote-syntax #,id)) imports))
            (untyped-invoke-unit/infer us.untyped-stx))))))]))

(define-syntax (invoke-unit stx)
  (syntax-parse stx
    [(invoke-unit unit-expr imports:invoke-imports)
     (ignore
      (tr:unit:invoke
       (quasisyntax/loc stx
         (#%expression
          (begin
            (void)
            (void #,@#'imports.imports)
            (untyped-invoke-unit
             #,(tr:unit:invoke:expr-property
                #'unit-expr
                #t)
             #,@(attribute imports.untyped-import)))))))]))


(define-syntax (add-tags stx)
  (syntax-parse stx
    [(_) #'(begin)]
    [(_ e)
     (define exp-e (local-expand #'e (syntax-local-context) (kernel-form-identifier-list)))
     (syntax-parse exp-e
       #:literals (begin define-values define-syntaxes :)
       [(begin b ...)
        #'(add-tags b ...)]
       [(define-syntaxes (name:id ...) rhs:expr)
        exp-e]
       [(define-values () (colon-helper (: name:id type) rest ...))
        #`(define-values ()
            #,(tr:unit:body-exp-def-type-property
               #'(#%expression
                  (begin (void (lambda () name))
                         (colon-helper (: name type) rest ...)))
               'def/type))]
       [(define-values (name:id ...) rhs)
        #`(define-values (name ...)
            #,(tr:unit:body-exp-def-type-property
               #'(#%expression
                  (begin
                    (void (lambda () name) ...)
                    rhs))
               'def/type))]
       [_
        (tr:unit:body-exp-def-type-property exp-e 'expr)])]
    [(_ e ...)
     #'(begin (add-tags e) ...)]))

(define-syntax (unit stx)
  (syntax-parse stx
    #:literals (import export)
    [(unit (import im ...) (export ex ...) init-depends:init-depend-form e:unit-expr ...)
     (ignore
      (tr:unit
       (quasisyntax/loc stx
         (untyped-unit
          (import im ...)
          (export ex ...)
          #,@(attribute init-depends.form)
          (add-tags e ...)))))]))


(define-trampolining-macro process-define-unit
  [(define-values (name:id ...) rhs)
   #`(define-values (name ...)
       #,(ignore
          (tr:unit
           #'rhs)))])

;; define-unit macro
(define-syntax (define-unit stx)
  (syntax-parse stx
    #:literals (import export)
    [(define-unit uid:id
       (import im ...)
       (export ex ...)
       init-depends:init-depend-form
       e:unit-expr ...)
     (quasisyntax/loc stx
       (process-define-unit 
        (untyped-define-unit uid
          (import im ...)
          (export ex ...)
          #,@(attribute init-depends.form)
          (add-tags e ...))))]))


;; Syntax classes and macro for typed compound-unit
(begin-for-syntax
  (define-syntax-class compound-unit-form
    #:literals (compound-unit)
    (pattern
     (~and stx
           (compound-unit imports:compound-imports
                          exports:compound-exports
                          links:compound-links))
     #:attr untyped-stx
     (ignore
      (tr:unit:compound-property
       (quasisyntax/loc #'stx
         (#%expression
          (begin
            (void #,@#'imports.import-link-ids #,@(attribute links.bound-link-ids))
            (void #,@#'imports.import-sig-ids #,@(attribute links.bound-sig-ids))
            (void #,@#'imports.import-link-ids)
            exports.export-link-ids
            (void)
            (untyped-compound-unit imports
                                   exports
                                   links.untyped-links))))
       #t))))
  (define-syntax-class compound-imports
    #:literals (import)
    (pattern (import lb:link-binding ...)
             #:with import-link-ids
             #'(lb.link-qs ...)
             #:with import-sig-ids
             #'(lb.sig-qs ...)
             #:with import-link-map #'(lb.link-map-elem ...)))
  (define-syntax-class compound-exports
    #:literals (export)
    (pattern (export l:id ...)
             #:with export-link-ids 
             #'(void (quote-syntax l) ...)))
  (define-syntax-class compound-links
    #:literals (link)
    (pattern (link ld:linkage-decl ...)
             #:with untyped-links
             #'(link ld.untyped-link-decl ...)
             #:attr bound-link-ids (apply append (map syntax->list 
                                                   (syntax->list 
                                                    #'(ld.bound-link-ids ...))))
             #:attr bound-sig-ids (apply append (map syntax->list
                                                     (syntax->list
                                                      #'(ld.bound-sig-ids ...))))))
  (define-syntax-class linkage-decl
    (pattern ((lb:link-binding ...)
              unit-expr:expr
              link-id:id ...)
             #:with bound-link-ids #'(lb.link-qs ...)
             #:with bound-sig-ids #'(lb.sig-qs ...)
             #:with untyped-link-decl
             #`((lb ...)
                #,(tr:unit:compound:expr-property
                   #`(#%expression
                      (begin
                        (void (quote-syntax lb.sig-id) ...)
                        (void (quote-syntax lb.link-id) ...)
                        (void (quote-syntax link-id) ...)
                        unit-expr)) 
                   #t)
                link-id ...)))
  (define-syntax-class link-binding
    (pattern (link-id:id : sig-id:id)
             #:with link-qs #'(quote-syntax link-id)
             #:with sig-qs #'(quote-syntax sig-id)
             #:with link-map-elem #'(link-id sig-id))))

(define-syntax (compound-unit stx)
  (syntax-parse stx
    [cu:compound-unit-form
     (attribute cu.untyped-stx)]))

(define-trampolining-macro (process-define-compound-unit links sigs im ex infer)
  [(define-values (name:id ...) rhs)
   #`(define-values (name ...)
       #,(ignore
          (tr:unit:compound-property #`(#%expression (begin links sigs im ex infer rhs)) #t)))])

(define-syntax (define-compound-unit stx)
  (syntax-parse stx
    [(_ uid 
        imports:compound-imports
        exports:compound-exports
        links:compound-links)
     (quasisyntax/loc stx
       (process-define-compound-unit
        (void #,@#'imports.import-link-ids #,@(attribute links.bound-link-ids))
        (void #,@#'imports.import-sig-ids #,@(attribute links.bound-sig-ids))
        (void #,@#'imports.import-link-ids)
        (void)
        exports.export-link-ids
        (untyped-define-compound-unit uid
                                      imports
                                      exports
                                      links.untyped-links)))]))

;; compound-unit/infer
(begin-for-syntax
  (define-syntax-class compound-infer-imports
    #:literals (import)
    (pattern (import im:infer-link-import ...)
             #:with import-link-ids
             #'(im.link-qs ...)
             #:with import-sig-ids
             #'(im.sig-qs ...)))
  
  (define-syntax-class compound-infer-exports
    #:literals (export)
    (pattern (export ex:infer-link-export ...)
             #:with export-links-or-sigs
             #'(void (quote-syntax ex.link-or-sig-id) ...)))
  
  (define-syntax-class compound-infer-links
    #:literals (link)
    (pattern (link lnk:infer-linkage-decl ...)
             #:attr bound-link-ids (apply append (map syntax->list 
                                                      (syntax->list 
                                                       #'(lnk.bound-link-ids ...))))
             #:attr bound-sig-ids (apply append (map syntax->list
                                                     (syntax->list
                                                      #'(lnk.bound-sig-ids ...))))
             #:with links-untyped 
             #'(link lnk.linkage-stx ...)
             #:attr link-table
             #`(void lnk.linkage-information ...)))
  
  (define-syntax-class infer-link-import
    (pattern sig-id:id
             #:with sig-qs #'(quote-syntax sig-id)
             #:with link-qs #`(quote-syntax #,(generate-temporary)))
    (pattern (link-id:id : sig-id:id)
             #:with link-qs #'(quote-syntax link-id)
             #:with sig-qs #'(quote-syntax sig-id)))
  
  (define-syntax-class infer-link-export
    (pattern link-or-sig-id:id))
  (define (build-linkage-info unit-id lb-sig-ids lb-link-ids link-ids)
    (match-define-values ((list (cons _ imports) ...) (list (cons _ exports) ...))
                         (unit-static-signatures unit-id unit-id))
    (define runtime-id
      (local-expand unit-id (syntax-local-context) (kernel-form-identifier-list)))
    (tr:unit:compound:expr-property
     #`(#%expression
        (begin
          (void #,@lb-sig-ids)
          (void #,@lb-link-ids)
          (void #,@link-ids)
          ;; all imports
          (void #,@(map (lambda (id) #`(quote-syntax #,id)) imports))
          ;; all exports
          (void #,@(map (lambda (id) #`(quote-syntax #,id)) exports))
          (void (quote-syntax #,runtime-id))))
     'infer))
  
  (define-syntax-class infer-linkage-decl
    (pattern ((lb:link-binding ...)
              unit-id:id
              link-id:id ...)
             #:with bound-link-ids #'(lb.link-qs ...)
             #:with bound-sig-ids #'(lb.sig-qs ...)
             #:with linkage-stx
             #`((lb ...)
                #,(tr:unit:compound:expr-property
                   #'unit-id
                   'infer)
                link-id ...)
             #:with linkage-information (build-linkage-info #'unit-id
                                                            #'((quote-syntax lb.sig-id) ...)
                                                            #'((quote-syntax lb.link-id) ...)
                                                            #'((quote-syntax link-id) ...)))
    (pattern unit-id:id
             #:with bound-link-ids #'()
             #:with bound-sig-ids #'()
             #:with linkage-stx
             (tr:unit:compound:expr-property #'unit-id 'infer)
             #:with linkage-information (build-linkage-info #'unit-id
                                                            #'()
                                                            #'()
                                                            #'()))))

;; NOTE/TODO:
;; - it seems that the docs for compound-unit/infer
;;   suggest that imports are filled in from the 
;;   static information bound to the unit-ids
;;   but simple tests don't seem to confirm this
;; The typed implementation similarly does not fill
;; in any import information into the `import` clause
(define-syntax (compound-unit/infer stx)
  (syntax-parse stx
    [(_ 
      imports:compound-infer-imports
      exports:compound-infer-exports
      links:compound-infer-links)
     (ignore
      (tr:unit:compound-property
       (quasisyntax/loc stx
         (#%expression
          (begin
            (void #,@#'imports.import-link-ids
                  #,@(attribute links.bound-link-ids))
            (void #,@#'imports.import-sig-ids
                  #,@(attribute links.bound-sig-ids))
            (void #,@#'imports.import-link-ids) 
            exports.export-links-or-sigs
            #,(attribute links.link-table)
            (untyped-compound-unit/infer
             imports
             exports 
             links))))
       'infer))]))

(define-syntax (define-compound-unit/infer stx)
  (syntax-parse stx
    [(_ unit-name:id
        imports:compound-infer-imports
        exports:compound-infer-exports
        links:compound-infer-links)
     (quasisyntax/loc stx
       (process-define-compound-unit
        (void #,@#'imports.import-link-ids
              #,@(attribute links.bound-link-ids))
        (void #,@#'imports.import-sig-ids
              #,@(attribute links.bound-sig-ids))
        (void #,@#'imports.import-link-ids) 
        exports.export-links-or-sigs
        #,(attribute links.link-table)
        (untyped-define-compound-unit/infer unit-name
                                            imports
                                            exports
                                            links)))]))

;; Ignoring renames/prefix/etc for now
(define-syntax (unit-from-context stx)
  (syntax-parse stx
    [(_ sig:id)
     (ignore
      (tr:unit:from-context-property
       (quasisyntax/loc stx
         (#%expression
          (begin
            (void (quote-syntax sig))
            (untyped-unit-from-context sig))))
       #t))]))
