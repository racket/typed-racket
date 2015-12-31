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
         require-typed-struct/provide cast make-predicate define-predicate
         require-typed-signature)

(module forms racket/base
  (require (for-syntax racket/lazy-require racket/base))
  (begin-for-syntax 
    (lazy-require [(submod "..")
                   (require/opaque-type
                    require-typed-signature
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
        require-typed-signature
        require-typed-struct-legacy
        require-typed-struct
        require/typed-legacy require/typed require/typed/provide
        require-typed-struct/provide cast make-predicate define-predicate))

;; unsafe operations go in this submodule
(module* unsafe #f
  ;; turned into a macro on the requiring side
  (provide -unsafe-require/typed))

;; used for private unsafe functionality in require macros
;; *do not export*
(define-syntax unsafe-kw (syntax-rules ()))

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
         racket/base
         racket/struct-info
         syntax/struct
         syntax/location
         (for-template "../utils/any-wrap.rkt")
         "../utils/tc-utils.rkt"
         "../private/syntax-properties.rkt"
         "../typecheck/internal-forms.rkt"
         ;; struct-extraction is actually used at both of these phases
         "../utils/struct-extraction.rkt"
         (for-syntax "../utils/struct-extraction.rkt")
         (only-in "../utils/utils.rkt" syntax-length)
         (for-template racket/base "ann-inst.rkt"))

;; Lazily loaded b/c they're only used sometimes, so we save a lot
;; of loading by not having them when they are unneeded
(lazy-require ["../rep/type-rep.rkt" (Error?)]
              ["../types/utils.rkt" (fv)]
              [typed-racket/private/parse-type (parse-type)])

(define (with-type* expr ty)
  (with-type #`(ann #,expr #,ty)))

(define (ignore-some/expr expr ty)
  #`(#,(ignore-some-expr-property #'#%expression ty) #,expr))

(define-syntax-class opt-parent
  #:attributes (nm parent)
  (pattern nm:id #:with parent #'#f)
  (pattern (nm:id parent:id)))


(define-values (require/typed-legacy require/typed -unsafe-require/typed)
 (let ()
  (define-syntax-class opt-rename
    #:attributes (nm orig-nm spec)
    (pattern nm:id
             #:with orig-nm #'nm
             #:with spec #'nm)
    (pattern (orig-nm:id internal-nm:id)
             #:with spec #'(orig-nm internal-nm)
             #:with nm #'internal-nm))

  (define-syntax-class simple-clause
    #:attributes (nm ty)
    (pattern [nm:opt-rename ty]))

  (define-splicing-syntax-class (struct-opts legacy struct-name)
     #:attributes (ctor-value type)
     (pattern (~seq (~optional (~seq (~and key (~or #:extra-constructor-name #:constructor-name))
                                     name:id))
                    (~optional (~seq #:type-name type:id) #:defaults ([type struct-name])))
              #:attr ctor-value (if (attribute key) #'(key name)
                                    (if legacy
                                        #`(#:extra-constructor-name
                                           #,(format-id struct-name "make-~a" struct-name))
                                        #'()))))

  (define-syntax-class (struct-clause legacy)
    #:attributes (nm type (body 1) (constructor-parts 1))
    (pattern [(~or (~datum struct) #:struct)
              nm:opt-parent (body ...)
              (~var opts (struct-opts legacy #'nm.nm))]
             #:with (constructor-parts ...) #'opts.ctor-value
             #:attr type #'opts.type))

  (define-syntax-class signature-clause
    #:literals (:)
    #:attributes (sig-name [var 1] [type 1])
    (pattern [#:signature sig-name:id ([var:id : type] ...)]))

  (define-syntax-class opaque-clause
    ;#:literals (opaque)
    #:attributes (ty pred opt)
    (pattern [(~or (~datum opaque) #:opaque) ty:id pred:id]
             #:with opt #'())
    (pattern [(~or (~datum opaque) #:opaque) opaque ty:id pred:id #:name-exists]
             #:with opt #'(#:name-exists)))

  (define-syntax-class (clause legacy unsafe? lib)
   #:attributes (spec)
   (pattern oc:opaque-clause #:attr spec
     #`(require/opaque-type oc.ty oc.pred #,lib . oc.opt))
   (pattern (~var strc (struct-clause legacy)) #:attr spec
     #`(require-typed-struct strc.nm (strc.body ...) strc.constructor-parts ...
                             #:type-name strc.type
                             #,@(if unsafe? #'(unsafe-kw) #'())
                             #,lib))
   (pattern sig:signature-clause #:attr spec
     #`(require-typed-signature sig.sig-name (sig.var ...) (sig.type ...) #,lib))
   (pattern sc:simple-clause #:attr spec
     #`(require/typed #:internal sc.nm sc.ty #,lib
                      #,@(if unsafe? #'(unsafe-kw) #'()))))


  (define ((r/t-maker legacy unsafe?) stx)
    (unless (or (unbox typed-context?) (eq? (syntax-local-context) 'module-begin))
      (raise-syntax-error #f "only allowed in a typed module" stx))
    (syntax-parse stx
      [(_ lib:expr (~var c (clause legacy unsafe? #'lib)) ...)
       (when (zero? (syntax-length #'(c ...)))
         (raise-syntax-error #f "at least one specification is required" stx))
       #`(begin c.spec ...)]
      [(_ #:internal nm:opt-rename ty lib
          (~optional [~seq #:struct-maker parent])
          (~optional (~and (~seq (~literal unsafe-kw))
                           (~bind [unsafe? #t]))
                     #:defaults ([unsafe? #f])))
       (define/with-syntax hidden (generate-temporary #'nm.nm))
       (define/with-syntax sm (if (attribute parent)
                                  #'(#:struct-maker parent)
                                  #'()))
       (cond [(not (attribute unsafe?))
              ;; define `cnt*` to be fixed up later by the module type-checking
              (define cnt*
                (syntax-local-lift-expression
                 (make-contract-def-rhs #'ty #f (attribute parent))))
              (quasisyntax/loc stx
                (begin
                  ;; register the identifier so that it has a binding (for top-level)
                  #,@(if (eq? (syntax-local-context) 'top-level)
                         (list #'(define-syntaxes (hidden) (values)))
                         null)
                  #,(internal #'(require/typed-internal hidden ty . sm))
                  #,(ignore #`(require/contract nm.spec hidden #,cnt* lib))))]
             [else
              (define/with-syntax hidden2 (generate-temporary #'nm.nm))
              (quasisyntax/loc stx
                (begin
                  (require (only-in lib [nm.orig-nm hidden]))
                  ;; need this indirection since `hidden` may expand
                  ;; to a different identifier that TR doesn't know about
                  #,(ignore #'(define hidden2 hidden))
                  (rename-without-provide nm.nm hidden2)
                  #,(internal #'(require/typed-internal hidden2 ty . sm))))])]))
  (values (r/t-maker #t #f) (r/t-maker #f #f) (r/t-maker #f #t))))


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
    [_ #:when (eq? 'module-begin (syntax-local-context))
       ;; it would be inconvenient to find the correct #%module-begin here, so we rely on splicing
       #`(begin #,stx (begin))]
    [(_ ty:id pred:id lib (~optional ne:name-exists-kw) ...)
     (with-syntax ([hidden (generate-temporary #'pred)])
       (define pred-cnt
         (syntax-local-lift-expression
          (make-contract-def-rhs #'(-> Any Boolean) #f #f)))
       (quasisyntax/loc stx
         (begin
           ;; register the identifier for the top-level (see require/typed)
           #,@(if (eq? (syntax-local-context) 'top-level)
                  (list #'(define-syntaxes (hidden) (values)))
                  null)
           #,(internal #'(require/typed-internal hidden (Any -> Boolean : (Opaque pred))))
           #,(if (attribute ne)
                 (internal (syntax/loc stx (define-type-alias-internal ty (Opaque pred))))
                 (syntax/loc stx (define-type-alias ty (Opaque pred))))
           #,(ignore #`(require/contract pred hidden #,pred-cnt lib)))))]))



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

  (define-splicing-syntax-class unsafe-clause
   (pattern (~seq) #:attr unsafe? #f)
   (pattern (~seq (~literal unsafe-kw)) #:attr unsafe? #t))

  (define ((rts legacy) stx)
    (syntax-parse stx #:literals (:)
      [(_ name:opt-parent
          ([fld : ty] ...)
          (~var input-maker (constructor-term legacy #'name.nm))
          (~optional (~seq #:type-name type:id) #:defaults ([type #'name.nm]))
          unsafe:unsafe-clause
          lib)
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

                         (dtsi* () spec type ([fld : ty] ...) #:maker maker-name #:type-only)
                         #,(ignore #'(require/contract pred hidden (or/c struct-predicate-procedure?/c (c-> any-wrap/c boolean?)) lib))
                         #,(internal #'(require/typed-internal hidden (Any -> Boolean : type)))
                         (require/typed #:internal (maker-name real-maker) type lib
                                        #:struct-maker parent
                                        #,@(if (attribute unsafe.unsafe?) #'(unsafe-kw) #'()))

                         ;This needs to be a different identifier to meet the specifications
                         ;of struct (the id constructor shouldn't expand to it)
                         #,(if (syntax-e #'extra-maker)
                               #`(require/typed #:internal (maker-name extra-maker) type lib
                                                #:struct-maker parent
                                                #,@(if (attribute unsafe.unsafe?) #'(unsafe-kw) #'()))
                               #'(begin))

                         #,(if (not (free-identifier=? #'nm #'type))
                               #'(define-syntax type
                                   (lambda (stx)
                                     (raise-syntax-error
                                      'type-check
                                      (format "type name ~a used out of context in ~a"
                                              (syntax->datum (if (stx-pair? stx)
                                                                 (stx-car stx)
                                                                 stx))
                                              (syntax->datum stx))
                                      stx
                                      (and (stx-pair? stx) (stx-car stx)))))
                               #'(begin))

                         #,@(if (attribute unsafe.unsafe?)
                                #'((require/typed #:internal sel (type -> ty) lib unsafe-kw) ...)
                                #'((require/typed lib [sel (type -> ty)]) ...)))))]))

  (values (rts #t) (rts #f))))

(define (require-typed-signature stx)
  (syntax-parse stx
    #:literals (:)
    [(_ sig-name:id (var ...) (type ...) lib)
     (quasisyntax/loc stx
       (begin
         (require (only-in lib sig-name))
         #,(internal (quasisyntax/loc stx
                       (define-signature-internal sig-name
                         #:parent-signature #f
                         ([var type] ...)
                         ;; infer parent relationships using the static information
                         ;; bound to this signature
                         #:check? #t)))))]))
