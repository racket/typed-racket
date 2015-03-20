#lang racket/base

;; This file defines primitives that make use of contracts in their
;; expansion. This include `cast` and various forms of
;; `require/typed`.
;;
;; Additionally, the _implementations_ of these forms are lazily
;; loaded. This works as follows:
;;
;; - the forms themselves as defined (using `define-syntax`) in the
;;   `forms` submodule
;;
;; - their implementations (under the same names) are defined at phase
;;   0 using `define` in the main module
;; 
;; - the `forms` submodule uses `lazy-require` to load the
;;   implementations of the forms


(provide require/opaque-type require-typed-struct-legacy require-typed-struct
         require/typed-legacy require/typed require/typed/provide
         require-typed-struct/provide cast make-predicate define-predicate)

(module forms racket/base
  (require (for-syntax racket/lazy-require racket/base))
  (begin-for-syntax 
    (lazy-require [(submod "..")
                   (require/opaque-type 
                    require-typed-struct-legacy
                    require-typed-struct
                    require/typed-legacy require/typed require/typed/provide
                    require-typed-struct/provide cast make-predicate define-predicate)]))
  (define-syntax (def stx)
    (syntax-case stx ()
      [(_ id ...)
       (with-syntax ([(names ...) (generate-temporaries #'(id ...))])
         #'(begin (provide (rename-out [names id] ...))
                  (define-syntax (names stx) (id stx)) ...))]))
  (def require/opaque-type 
        require-typed-struct-legacy
        require-typed-struct
        require/typed-legacy require/typed require/typed/provide
        require-typed-struct/provide cast make-predicate define-predicate))

(require (for-template (submod "." forms) "../utils/require-contract.rkt"
                       (submod "../typecheck/internal-forms.rkt" forms)
                       "colon.rkt"
                       "top-interaction.rkt"
                       "base-types.rkt"
                       "base-types-extra.rkt"
                       "prims-struct.rkt"
                       syntax/location
                       (rename-in racket/contract/base [-> c->] [->* c->*] [case-> c:case->])))

(require racket/lazy-require
         syntax/parse/pre
         syntax/stx
         racket/syntax
         unstable/syntax
         racket/base
         racket/struct-info
         syntax/struct
         syntax/location
         "../utils/tc-utils.rkt"
         "../private/syntax-properties.rkt"
         "../typecheck/internal-forms.rkt"
         ;; struct-extraction is actually used at both of these phases
         "../utils/struct-extraction.rkt"
         (for-syntax "../utils/struct-extraction.rkt")
         (for-template racket/base "ann-inst.rkt"))

;; Lazily loaded b/c they're only used sometimes, so we save a lot
;; of loading by not having them when they are unneeded
(lazy-require ["../rep/type-rep.rkt" (make-Opaque Error?)]
              ["../types/utils.rkt" (fv)]
              [syntax/define (normalize-definition)]
              [typed-racket/private/parse-type (parse-type)]
              [typed-racket/env/type-alias-env (register-resolved-type-alias)])

(define (with-type* expr ty)
  (with-type #`(ann #,expr #,ty)))

(define (ignore-some/expr expr ty)
  #`(#,(ignore-some-expr-property #'#%expression ty) #,expr))

(define-syntax-class opt-parent
  #:attributes (nm parent)
  (pattern nm:id #:with parent #'#f)
  (pattern (nm:id parent:id)))


(define-values (require/typed-legacy require/typed)
 (let ()
  (define-syntax-class opt-rename
    #:attributes (nm spec)
    (pattern nm:id
             #:with spec #'nm)
    (pattern (orig-nm:id internal-nm:id)
             #:with spec #'(orig-nm internal-nm)
             #:with nm #'internal-nm))

  (define-syntax-class simple-clause
    #:attributes (nm ty)
    (pattern [nm:opt-rename ty]))

  (define-splicing-syntax-class (opt-constructor legacy struct-name)
   #:attributes (value)
   (pattern (~seq)
            #:attr value (if legacy
                             #`(#:extra-constructor-name
                                #,(format-id struct-name "make-~a" struct-name))
                             #'()))
   (pattern (~seq (~and key (~or #:extra-constructor-name #:constructor-name)) name:id)
            #:attr value #'(key name)))

  (define-syntax-class (struct-clause legacy)
    ;#:literals (struct)
    #:attributes (nm (body 1) (constructor-parts 1))
    (pattern [(~or (~datum struct) #:struct)
              nm:opt-parent (body ...)
              (~var constructor (opt-constructor legacy #'nm.nm))]
             #:with (constructor-parts ...) #'constructor.value))

  (define-syntax-class opaque-clause
    ;#:literals (opaque)
    #:attributes (ty pred opt)
    (pattern [(~or (~datum opaque) #:opaque) ty:id pred:id]
             #:with opt #'())
    (pattern [(~or (~datum opaque) #:opaque) opaque ty:id pred:id #:name-exists]
             #:with opt #'(#:name-exists)))

  (define-syntax-class (clause legacy lib)
   #:attributes (spec)
   (pattern oc:opaque-clause #:attr spec
     #`(require/opaque-type oc.ty oc.pred #,lib . oc.opt))
   (pattern (~var strc (struct-clause legacy)) #:attr spec
     #`(require-typed-struct strc.nm (strc.body ...) strc.constructor-parts ... #,lib))
   (pattern sc:simple-clause #:attr spec
     #`(require/typed #:internal sc.nm sc.ty #,lib)))


  (define ((r/t-maker legacy) stx)
    (syntax-parse stx
      [(_ lib:expr (~var c (clause legacy #'lib)) ...)
       (when (zero? (syntax-length #'(c ...)))
         (raise-syntax-error #f "at least one specification is required" stx))
       #`(begin c.spec ...)]
      [(_ #:internal nm:opt-rename ty lib (~optional [~seq #:struct-maker parent]) ...)
       (define/with-syntax hidden (generate-temporary #'nm.nm))
       (define/with-syntax sm (if (attribute parent)
                                  #'(#:struct-maker parent)
                                  #'()))
       ;; define `cnt*` to be fixed up later by the module type-checking
       (define cnt*
         (syntax-local-lift-expression
          (make-contract-def-rhs #'ty #f (attribute parent))))
       (quasisyntax/loc stx
         (begin
           #,(internal #'(require/typed-internal hidden ty . sm))
           #,(ignore #`(require/contract nm.spec hidden #,cnt* lib))))]))
  (values (r/t-maker #t) (r/t-maker #f))))


(define (require/typed/provide stx)
  (unless (memq (syntax-local-context) '(module module-begin))
    (raise-syntax-error 'require/typed/provide
                        "can only be used at module top-level"))
  (syntax-parse stx
    [(_ lib) #'(begin)]
    [(_ lib [r:id t] other-clause ...)
     #'(begin (require/typed lib [r t])
              (provide r)
              (require/typed/provide lib other-clause ...))]
    [(_ lib (~and clause [#:struct name:id ([f:id (~datum :) t] ...)
                                   option ...])
        other-clause ...)
     #'(begin (require/typed lib clause)
              (provide (struct-out name))
              (require/typed/provide lib other-clause ...))]
    [(_ lib (~and clause [#:struct (name:id parent:id)
                                   ([f:id (~datum :) t] ...)
                                   option ...])
        other-clause ...)
     #'(begin (require/typed lib clause)
              (provide (struct-out name))
              (require/typed/provide lib other-clause ...))]
    [(_ lib (~and clause [#:opaque t:id pred:id])
        other-clause ...)
     #'(begin (require/typed lib clause)
              (provide t pred)
              (require/typed/provide lib other-clause ...))]))


(define require-typed-struct/provide
  (syntax-rules ()
    [(_ (nm par) . rest)
     (begin (require-typed-struct (nm par) . rest)
            (provide (struct-out nm)))]
    [(_ nm . rest)
     (begin (require-typed-struct nm . rest)
            (provide (struct-out nm)))]))

;; Conversion of types to contracts
;;  define-predicate
;;  make-predicate
;;  cast

;; Helper to construct syntax for contract definitions
(define (make-contract-def-rhs type flat? maker?)
  (contract-def-property #'#f `#s(contract-def ,type ,flat? ,maker? untyped)))

(define (define-predicate stx)
  (syntax-parse stx
    [(_ name:id ty:expr)
     #`(begin
         ;; We want the value bound to name to have a nice object name. Using the built in mechanism
         ;; of define has better performance than procedure-rename.
         #,(ignore
             #'(define name
                 (let ([pred (make-predicate ty)])
                   (lambda (x) (pred x)))))
         ;; not a require, this is just the unchecked declaration syntax
         #,(internal #'(require/typed-internal name (Any -> Boolean : ty))))]))


(define (make-predicate stx)
  (syntax-parse stx
    [(_ ty:expr)
     (define name (syntax-local-lift-expression
                   (make-contract-def-rhs #'ty #t #f)))
     (define (check-valid-type _)
       (define type (parse-type #'ty))
       (define vars (fv type))
       ;; If there was an error don't create another one
       (unless (or (Error? type) (null? vars))
         (tc-error/delayed
          "Type ~a could not be converted to a predicate because it contains free variables."
          type)))
     #`(#,(external-check-property #'#%expression check-valid-type)
        #,(ignore-some/expr #`(flat-contract-predicate #,name) #'(Any -> Boolean : ty)))]))


(define (cast stx)
  (syntax-parse stx
    [(_ v:expr ty:expr)
     (define (apply-contract ctc-expr)
       #`(#%expression
          #,(ignore-some/expr
             #`(let-values (((val) #,(with-type* #'v #'Any)))
                 #,(syntax-property
                    (quasisyntax/loc stx
                      (contract
                       #,ctc-expr
                       val
                       'cast
                       'typed-world
                       val
                       (quote-srcloc #,stx)))
                    'feature-profile:TR-dynamic-check #t))
             #'ty)))

     (cond [(not (unbox typed-context?)) ; no-check, don't check
            #'v]
           [else
            (define ctc (syntax-local-lift-expression
                         (make-contract-def-rhs #'ty #f #f)))
            (define (check-valid-type _)
              (define type (parse-type #'ty))
              (define vars (fv type))
              ;; If there was an error don't create another one
              (unless (or (Error? type) (null? vars))
                (tc-error/delayed
                 "Type ~a could not be converted to a contract because it contains free variables."
                 type)))
            #`(#,(external-check-property #'#%expression check-valid-type)
               #,(apply-contract ctc))])]))



(define (require/opaque-type stx)
  (define-syntax-class name-exists-kw
    (pattern #:name-exists))
  (syntax-parse stx
    [(_ ty:id pred:id lib (~optional ne:name-exists-kw) ...)
     ;; This line appears redundant with the use of `define-type-alias` below, but
     ;; it's actually necessary for top-level uses because this opaque type may appear
     ;; in subsequent `require/typed` clauses, which needs to parse the types at
     ;; expansion-time, not at typechecking time when aliases are installed.
     (register-resolved-type-alias #'ty (make-Opaque #'pred))
     (with-syntax ([hidden (generate-temporary #'pred)])
       (quasisyntax/loc stx
         (begin
           #,(ignore #'(define pred-cnt (any/c . c-> . boolean?)))
           #,(internal #'(require/typed-internal hidden (Any -> Boolean : (Opaque pred))))
           #,(if (attribute ne)
                 (internal (syntax/loc stx (define-type-alias-internal ty (Opaque pred))))
                 (syntax/loc stx (define-type-alias ty (Opaque pred))))
           #,(ignore #'(require/contract pred hidden pred-cnt lib)))))]))



(module self-ctor racket/base
  (require racket/struct-info)

  ;Copied from racket/private/define-struct
  ;FIXME when multiple bindings are supported
  (define (self-ctor-transformer orig stx)
    (define (transfer-srcloc orig stx)
      (datum->syntax orig (syntax-e orig) stx orig))
    (syntax-case stx ()
      [(self arg ...) (datum->syntax stx
                                     (cons (syntax-property (transfer-srcloc orig #'self)
                                                            'constructor-for
                                                            (syntax-local-introduce #'self))
                                           (syntax-e (syntax (arg ...))))
                                     stx
                                     stx)]
      [_ (transfer-srcloc orig stx)]))
  (define make-struct-info-self-ctor
    (let ()
      (struct struct-info-self-ctor (id info)
        #:property prop:procedure
        (lambda (ins stx)
          (self-ctor-transformer (struct-info-self-ctor-id ins) stx))
        #:property prop:struct-info (λ (x) (extract-struct-info (struct-info-self-ctor-info x))))
      struct-info-self-ctor))
  (provide make-struct-info-self-ctor))

(require (submod "." self-ctor))



(define-values (require-typed-struct-legacy require-typed-struct)
 (let ()

  (define-splicing-syntax-class (constructor-term legacy struct-name)
   (pattern (~seq) #:fail-when legacy #f #:attr name struct-name #:attr extra #f)
   (pattern (~seq) #:fail-unless legacy #f #:attr name (format-id struct-name "make-~a" struct-name)
            #:attr extra #t)
   (pattern (~seq #:constructor-name name:id) #:attr extra #f)
   (pattern (~seq #:extra-constructor-name name:id) #:attr extra #t))


  (define ((rts legacy) stx)
    (syntax-parse stx #:literals (:)
      [(_ name:opt-parent ([fld : ty] ...) (~var input-maker (constructor-term legacy #'name.nm)) lib)
       (with-syntax* ([nm #'name.nm]
                      [parent #'name.parent]
                      [hidden (generate-temporary #'name.nm)]
                      [orig-struct-info (generate-temporary #'nm)]
                      [spec (if (syntax-e #'name.parent) #'(nm parent) #'nm)]
                      [num-fields (syntax-length #'(fld ...))]
                      [(type-des _ pred sel ...)
                       (build-struct-names #'nm (syntax->list #'(fld ...)) #f #t)]
                      [(mut ...) (stx-map (lambda _ #'#f) #'(sel ...))]
                      [maker-name #'input-maker.name]
                      ;maker-name's symbolic form is used in the require form
                      [id-is-ctor? (or (attribute input-maker.extra)
                                       (bound-identifier=? #'maker-name #'nm))]
                      ;Only used if id-is-ctor? is true
                      [internal-maker (generate-temporary #'maker-name)]
                      ;The actual identifier bound to the constructor
                      [real-maker (if (syntax-e #'id-is-ctor?) #'internal-maker #'maker-name)]
                      [extra-maker (and (attribute input-maker.extra)
                                        (not (bound-identifier=? #'make-name #'nm))
                                        #'maker-name)])
                     (define (maybe-add-quote-syntax stx)
                       (if (and stx (syntax-e stx)) #`(quote-syntax #,stx) stx))

                     (quasisyntax/loc stx
                       (begin
                         (require (only-in lib type-des (nm orig-struct-info)))

                         (define-for-syntax si
                           (let ()
                             (define-values (orig-type-des orig-maker orig-pred
                                             orig-sels orig-muts orig-parent)
                               (apply values (extract-struct-info/checked
                                              (quote-syntax orig-struct-info))))

                             (define (id-drop sels muts num)
                               (cond
                                [(zero? num) (values sels muts)]
                                [(null? sels) (int-err "id-drop: Too short of list")]
                                [(pair? sels)
                                 (cond
                                   [(not (car sels)) (values sels muts)]
                                   [else (id-drop (cdr sels) (cdr muts) (sub1 num))])]
                                [else (int-err "id-drop: Not a list")]))

                             (define (struct-info-list new-sels new-muts)
                               (list (quote-syntax type-des)
                                     (quote-syntax real-maker)
                                     (quote-syntax pred)
                                     (append (list #,@(map maybe-add-quote-syntax
                                                           (reverse (syntax->list #'(sel ...)))))
                                             new-sels)
                                     (append (list #,@(map maybe-add-quote-syntax
                                                           (reverse (syntax->list #'(mut ...)))))
                                             new-muts)
                                     orig-parent))

                             (make-struct-info
                               (lambda ()
                                 #,(if (syntax-e #'parent)
                                       (let-values (((parent-type-des parent-maker parent-pred
                                                      parent-sel  parent-mut grand-parent)
                                                     (apply values
                                                            (extract-struct-info/checked #'parent))))
                                         #`(struct-info-list
                                             (list #,@(map maybe-add-quote-syntax parent-sel))
                                             (list #,@(map maybe-add-quote-syntax parent-mut))))
                                       #`(let-values (((new-sels new-muts)
                                                       (id-drop orig-sels orig-muts num-fields)))
                                           (struct-info-list new-sels new-muts)))))))

                         (define-syntax nm
                              (if id-is-ctor?
                                  (make-struct-info-self-ctor #'internal-maker si)
                                  si))

                         (dtsi* () spec ([fld : ty] ...) #:maker maker-name #:type-only)
                         #,(ignore #'(require/contract pred hidden (any/c . c-> . boolean?) lib))
                         #,(internal #'(require/typed-internal hidden (Any -> Boolean : nm)))
                         (require/typed #:internal (maker-name real-maker) nm lib
                                        #:struct-maker parent)

                         ;This needs to be a different identifier to meet the specifications
                         ;of struct (the id constructor shouldn't expand to it)
                         #,(if (syntax-e #'extra-maker)
                               #'(require/typed #:internal (maker-name extra-maker) nm lib
                                                #:struct-maker parent)
                               #'(begin))

                         (require/typed lib
                           [sel (nm -> ty)]) ...)))]))

  (values (rts #t) (rts #f))))
