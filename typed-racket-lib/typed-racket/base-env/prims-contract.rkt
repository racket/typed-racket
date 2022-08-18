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


(provide require/opaque-type require/opaque-type-shallow require/opaque-type-optional
         require-typed-struct-legacy require-typed-struct-legacy-shallow require-typed-struct-legacy-optional
         require-typed-struct require-typed-struct-shallow require-typed-struct-optional
         require/typed-legacy require/typed-legacy-shallow require/typed-legacy-optional
         require/typed require/typed-shallow require/typed-optional
         require/typed/provide require/typed/provide-shallow require/typed/provide-optional
         require-typed-struct/provide require-typed-struct/provide-shallow require-typed-struct/provide-optional
         core-cast core-cast-shallow core-cast-optional
         make-predicate
         define-predicate
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
                    require-typed-struct/provide core-cast make-predicate define-predicate)]))
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
       require-typed-struct/provide make-predicate define-predicate)

  ;; Expand `cast` to a `core-cast` with an extra `#%expression` in order
  ;; to prevent the contract generation pass from executing too early
  ;; (i.e., before the `cast` typechecks)
  (define-syntax (-core-cast stx) (core-cast stx))
  (define-syntax (cast stx)
    (syntax-case stx ()
      [(_ e ty) (quasisyntax/loc stx (#%expression #,(syntax/loc stx (-core-cast e ty))))]))
  (provide cast))

;; copied from `forms`
(module forms-shallow racket/base
  (require (for-syntax racket/lazy-require racket/base))
  (begin-for-syntax
    (lazy-require [(submod "..")
                   (require/opaque-type-shallow
                    (require-typed-signature require-typed-signature-shallow)
                    require-typed-struct-legacy-shallow
                    require-typed-struct-shallow
                    require/typed-legacy-shallow
                    require/typed-shallow
                    require/typed/provide-shallow
                    require-typed-struct/provide-shallow
                    core-cast-shallow)]))
  (define-syntax (def stx)
    (syntax-case stx ()
      [(_ id ...)
       (with-syntax ([(names ...) (generate-temporaries #'(id ...))])
         #'(begin (provide (rename-out [names id] ...))
                  (define-syntax (names stx) (id stx)) ...))]))
  (def require/opaque-type-shallow
       require-typed-signature-shallow
       require-typed-struct-legacy-shallow
       require-typed-struct-shallow
       require/typed-legacy-shallow require/typed-shallow require/typed/provide-shallow
       require-typed-struct/provide-shallow)
  (define-syntax (-core-cast-shallow stx) (core-cast-shallow stx))
  (define-syntax (cast-shallow stx)
    (syntax-case stx ()
      [(_ e ty) (quasisyntax/loc stx (#%expression #,(syntax/loc stx (-core-cast-shallow e ty))))]))
  (provide cast-shallow))

;; copied from `forms`
(module forms-optional racket/base
  (require (for-syntax racket/lazy-require racket/base))
  (begin-for-syntax
    (lazy-require [(submod "..")
                   (require/opaque-type-optional
                    (require-typed-signature require-typed-signature-optional)
                    require-typed-struct-legacy-optional
                    require-typed-struct-optional
                    require/typed-legacy-optional
                    require/typed-optional
                    require/typed/provide-optional
                    require-typed-struct/provide-optional
                    core-cast-optional)]))
  (define-syntax (def stx)
    (syntax-case stx ()
      [(_ id ...)
       (with-syntax ([(names ...) (generate-temporaries #'(id ...))])
         #'(begin (provide (rename-out [names id] ...))
                  (define-syntax (names stx) (id stx)) ...))]))
  (def require/opaque-type-optional
       require-typed-signature-optional
       require-typed-struct-legacy-optional
       require-typed-struct-optional
       require/typed-legacy-optional require/typed-optional require/typed/provide-optional
       require-typed-struct/provide-optional)
  (define-syntax (-core-cast-optional stx) (core-cast-optional stx))
  (define-syntax (cast-optional stx)
    (syntax-case stx ()
      [(_ e ty) (quasisyntax/loc stx (#%expression #,(syntax/loc stx (-core-cast-optional e ty))))]))
  (provide cast-optional))


;; unsafe operations go in this submodule
(module* unsafe #f
  ;; turned into a macro on the requiring side
  (provide unsafe-require/typed))

;; used for private unsafe functionality in require macros
;; *do not export*
(define-syntax unsafe-kw (syntax-rules ()))

(require (for-template (submod "." forms)
                       (submod "." forms-shallow)
                       (submod "." forms-optional)
                       "../utils/require-contract.rkt"
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
         racket/function
         racket/base
         racket/struct-info
         syntax/struct
         syntax/location
         (only-in syntax/srcloc build-source-location-list)
         "../utils/require-contract.rkt"
         (for-template "../utils/any-wrap.rkt" "../utils/shallow-contract.rkt")
         "../utils/tc-utils.rkt"
         "../private/syntax-properties.rkt"
         "../private/cast-table.rkt"
         "../private/type-contract.rkt"
         "../typecheck/internal-forms.rkt"
         ;; struct-info is actually used at both of these phases
         "../utils/struct-info.rkt"
         (for-syntax "../utils/struct-info.rkt"
                     "type-name-error.rkt")
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
  #:commit
  #:attributes (nm parent)
  (pattern nm:id #:with parent #'#f)
  (pattern (nm:id parent:id)))

(define-syntax-class typed-field
  #:commit
  #:attributes (field type)
  #:datum-literals (:)
  (pattern [field:id : type]))

(define (require/contract-maker te-mode)
  (case te-mode
    ((shallow) #'require/contract-shallow)
    ((optional) #'require/contract-optional)
    (else #'require/contract)))

(define-values (require/typed-legacy require/typed-legacy-shallow require/typed-legacy-optional
                require/typed require/typed-shallow require/typed-optional
                unsafe-require/typed)
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
    #:attributes (nm type (body 1) (constructor-parts 1) (tvar 1))
    (pattern [(~or (~datum struct) #:struct)
              (~optional (~seq (tvar ...)) #:defaults ([(tvar 1) '()]))
              nm:opt-parent (body:typed-field ...)
              (~var opts (struct-opts legacy #'nm.nm))]
             #:with (constructor-parts ...) #'opts.ctor-value
             #:attr type #'opts.type))

  (define-syntax-class signature-clause
    #:literals (:)
    #:attributes (sig-name [var 1] [type 1])
    (pattern [#:signature sig-name:id (body:typed-field ...)]
      #:with (var ...) #'(body.field ...)
      #:with (type ...) #'(body.type ...)))

  (define-syntax-class opaque-clause
    ;#:literals (opaque)
    #:attributes (ty pred opt)
    (pattern [(~or (~datum opaque) #:opaque) ty:id pred:id]
             #:with opt #'())
    (pattern [(~or (~datum opaque) #:opaque) opaque ty:id pred:id #:name-exists]
             #:with opt #'(#:name-exists)))

  (define-syntax-class (clause legacy unsafe? te-mode lib)
   #:attributes (spec)
   (pattern oc:opaque-clause
     #:with r/ot/te-mode (case te-mode ((shallow) #'require/opaque-type-shallow) ((optional) #'require/opaque-type-optional) (else #'require/opaque-type))
     #:attr spec
     #`(r/ot/te-mode oc.ty oc.pred #,lib #,@(if unsafe? #'(unsafe-kw) #'()) . oc.opt))
   (pattern (~var strc (struct-clause legacy))
     #:with rts/te-mode (case te-mode ((shallow) #'require-typed-struct-shallow) ((optional) #'require-typed-struct-optional) (else #'require-typed-struct))
     #:attr spec
     #`(rts/te-mode strc.nm (strc.tvar ...)
                    (strc.body ...) strc.constructor-parts ...
                    #:type-name strc.type
                    #,@(if unsafe? #'(unsafe-kw) #'())
                    #,lib))
   (pattern sig:signature-clause #:attr spec
     (quasisyntax/loc #'sig (require-typed-signature sig.sig-name (sig.var ...) (sig.type ...) #,lib)))
   (pattern sc:simple-clause
     #:with r/t/te-mode (case te-mode ((shallow) #'require/typed-shallow) ((optional) #'require/typed-optional) (else #'require/typed))
     #:attr spec
     #`(r/t/te-mode #:internal sc.nm sc.ty #,lib
                    #,@(if unsafe? #'(unsafe-kw) #'()))))


  (define ((r/t-maker legacy unsafe? te-mode) stx)
    (unless (or (unbox typed-context?) (eq? (syntax-local-context) 'module-begin))
      (raise-syntax-error #f "only allowed in a typed module" stx))
    (syntax-parse stx
      [(_ lib:expr (~var c (clause legacy unsafe? te-mode #'lib)) ...)
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
       (define/with-syntax r/c/te-mode (require/contract-maker te-mode))
       (cond [(not (attribute unsafe?))
              ;; define `cnt*` to be fixed up later by the module type-checking
              (define cnt*
                (syntax-local-lift-expression
                 (make-contract-def-rhs #'ty #f (attribute parent) te-mode)))
              (quasisyntax/loc stx
                (begin
                  ;; register the identifier so that it has a binding (for top-level)
                  #,@(if (eq? (syntax-local-context) 'top-level)
                         (list #'(define-syntaxes (hidden) (values)))
                         null)
                  #,(internal #'(require/typed-internal hidden ty . sm))
                  #,(ignore #`(r/c/te-mode nm.spec hidden #,cnt* lib #,(format "~a" (syntax->datum #'ty))))))]
             [else
              (define/with-syntax hidden2 (generate-temporary #'nm.nm))
              (quasisyntax/loc stx
                (begin
                  (require (only-in lib [nm.orig-nm hidden]))
                  ;; need this indirection since `hidden` may expand
                  ;; to a different identifier that TR doesn't know about
                  #,(ignore #'(define hidden2 hidden))
                  (rename-without-provide nm.nm hidden2 hidden)
                  #,(internal #'(require/typed-internal hidden2 ty . sm))))])]))
  (values (r/t-maker #t #f deep) (r/t-maker #t #f shallow) (r/t-maker #t #f optional)
          (r/t-maker #f #f deep) (r/t-maker #f #f shallow) (r/t-maker #f #f optional)
          (r/t-maker #f #t deep))))

(define-values [require/typed/provide require/typed/provide-shallow require/typed/provide-optional]
  (let ()
    (define ((rtp-maker te-mode) stx)
      (unless (memq (syntax-local-context) '(module module-begin))
        (raise-syntax-error 'require/typed/provide
                            "can only be used at module top-level"))
      (define/with-syntax r/t/p (case te-mode ((shallow) #'require/typed/provide-shallow) ((optional) #'require/typed/provide-optional) (else #'require/typed/provide)))
      (define/with-syntax r/t (case te-mode ((shallow) #'require/typed-shallow) ((optional) #'require/typed-optional) (else #'require/typed)))
      (syntax-parse stx
        [(_ lib) #'(begin)]
        [(_ lib [r:id t] other-clause ...)
         #'(begin (r/t lib [r t])
                  (provide r)
                  (r/t/p lib other-clause ...))]
        [(_ lib (~and clause [#:struct nm:opt-parent
                                       (body:typed-field ...)
                                       option ...])
            other-clause ...)
         #'(begin (r/t lib clause)
                  (provide (struct-out nm.nm))
                  (r/t/p lib other-clause ...))]
        [(_ lib (~and clause [#:opaque t:id pred:id])
            other-clause ...)
         #'(begin (r/t lib clause)
                  (provide t pred)
                  (r/t/p lib other-clause ...))]))

    (values (rtp-maker deep) (rtp-maker shallow) (rtp-maker optional))))

(define-values [require-typed-struct/provide require-typed-struct/provide-shallow require-typed-struct/provide-optional]
  (let ()
    (define (rtsp-maker te-mode)
      (define/with-syntax r/t/s (case te-mode ((shallow) #'require-typed-struct-shallow) ((optional) #'require-typed-struct-optional) (else #'require-typed-struct)))
      (syntax-rules ()
        [(_ (nm par) . rest)
         (begin (r/t/s (nm par) . rest)
                (provide (struct-out nm)))]
        [(_ nm . rest)
         (begin (r/t/s nm . rest)
                (provide (struct-out nm)))]))
    (values (rtsp-maker 'guarder) (rtsp-maker 'shallow) (rtsp-maker 'optional))))

;; Conversion of types to contracts
;;  define-predicate
;;  make-predicate
;;  cast

;; Helpers to construct syntax for contract definitions
;; make-contract-def-rhs : Type-Stx Boolean Boolean -> Syntax
(define (make-contract-def-rhs type flat? maker? te-mode)
  (define contract-def `#s(contract-def ,type ,flat? ,maker? untyped ,te-mode))
  (contract-def-property #'#f (λ () contract-def)))

;; make-contract-def-rhs/from-typed : Id Boolean Boolean -> Syntax
(define (make-contract-def-rhs/from-typed id flat? maker? te-mode)
  (contract-def-property
   #'#f
   ;; This function should only be called after the type-checking pass has finished.
   ;; By then `tc/#%expression` will have recognized the `casted-expr` property and
   ;; will have added the casted expression's original type to the cast-table, so
   ;; that `(cast-table-ref id)` can get that type here.
   (λ ()
     (define type-stx
       (let ([types (cast-table-ref id)])
         (cond [(not types) #f]
               [(null? (cdr types)) (car types)]
               [else (quasisyntax/loc (car types) (U #,@types))])))
     `#s(contract-def ,type-stx ,flat? ,maker? typed ,te-mode))))

(define define-predicate
  (let ()
    (define (dp-maker te-mode)
      (define/with-syntax m-p (case te-mode ((shallow) #'make-predicate-shallow) ((optional) #'make-predicate-optional) (else #'make-predicate)))
      (lambda (stx)
        (syntax-parse stx
          [(_ name:id ty:expr)
           #`(begin
               ;; We want the value bound to name to have a nice object name. Using the built in mechanism
               ;; of define has better performance than procedure-rename.
               #,(ignore
                  (syntax/loc stx
                    (define name
                      (let ([pred (m-p ty)])
                        (lambda (x) (pred x))))))
               ;; not a require, this is just the unchecked declaration syntax
               #,(internal (syntax/loc stx (require/typed-internal name (Any -> Boolean : ty)))))])))
    (values (dp-maker deep)
            #;(dp-maker shallow)
            #;(dp-maker optional))))

(define make-predicate
  (let ()
    (define (mp-maker te-mode)
      (syntax-parser
        [(_ ty:expr)
         (define name (syntax-local-lift-expression
                       (make-contract-def-rhs #'ty #t #f te-mode)))
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
    (values (mp-maker deep)
            #;(mp-maker shallow) 
            #;(mp-maker optional))))

(define-values [core-cast core-cast-shallow core-cast-optional]
  (let ()
    (define ((cc-maker te-mode) stx)
      (syntax-parse stx
        [(_ v:expr ty:expr)
         (cond
           [(not (unbox typed-context?)) ; no-check, don't check
            #'v]
           [else
            (define new-ty-ctc (syntax-local-lift-expression
                                (make-contract-def-rhs #'ty #f #f te-mode)))
            (define existing-ty-id new-ty-ctc)
            (define existing-ty-ctc
              (syntax-local-lift-expression
                (make-contract-def-rhs/from-typed existing-ty-id #f #f te-mode)))
            (define (store-existing-type existing-type)
              (check-no-free-vars existing-type #'v)
              (cast-table-add! existing-ty-id (datum->syntax #f existing-type #'v)))
            (define (check-valid-type _)
              (define type (parse-type #'ty))
              (check-no-free-vars type #'ty))
            (define (check-no-free-vars type stx)
              (define vars (fv type))
              ;; If there was an error don't create another one
              (unless (or (Error? type) (null? vars))
                (tc-error/delayed
                 #:stx stx
                 "Type ~a could not be converted to a contract because it contains free variables."
                 type)))
            (define/with-syntax check-ty-expr (external-check-property #'#%expression check-valid-type))
            (define/with-syntax store-ty-expr (casted-expr-property #'#%expression store-existing-type))
            (case te-mode
              [(shallow)
               (define/with-syntax ty-str (format "~a" (syntax->datum #'ty))) ;;bg better to parse-type ?
               ;; (printf "damn ~a ~s ~a~n" (syntax-line stx) stx (build-source-location-list stx))
               (define/with-syntax ctx (build-source-location-list stx))
               #`(check-ty-expr
                  #,(ignore-some/expr
                      #`(#%plain-app shallow-shape-check
                                     (store-ty-expr v)
                                     #,new-ty-ctc 'ty-str 'ctx)
                      #'ty))]
              [(optional)
               #`(check-ty-expr
                  #,(ignore-some/expr
                      #`(store-ty-expr v)
                      #'ty))]
              [else
               (define (apply-contract v ctc-expr pos neg)
                 #`(#%expression
                    #,(ignore-some/expr
                       #`(let-values (((val) #,(with-type* v #'Any)))
                           #,(syntax-property
                              (quasisyntax/loc stx
                                (contract
                                 #,ctc-expr
                                 val
                                 '#,pos
                                 '#,neg
                                 #f
                                 (quote-srcloc #,stx)))
                              'feature-profile:TR-dynamic-check #t))
                       #'ty)))
               #`(check-ty-expr
                  #,(apply-contract
                     (apply-contract
                      #`(store-ty-expr
                         v)
                      existing-ty-ctc 'typed-world 'cast)
                     new-ty-ctc 'cast 'typed-world))])])]))
    (values (cc-maker deep) (cc-maker shallow) (cc-maker optional))))

(define-values [require/opaque-type require/opaque-type-shallow require/opaque-type-optional]
  (let ()
    (define-syntax-class unsafe-id
      (pattern (~literal unsafe-kw)))
    (define-syntax-class name-exists-kw
      (pattern #:name-exists))
    (define ((ro-maker te-mode) stx)
      (syntax-parse stx
        [_ #:when (eq? 'module-begin (syntax-local-context))
           ;; it would be inconvenient to find the correct #%module-begin here, so we rely on splicing
           #`(begin #,stx (begin))]
        [(_ ty:id pred:id lib (~optional unsafe:unsafe-id) (~optional ne:name-exists-kw) ...)
         (with-syntax ([hidden (generate-temporary #'pred)]
                       [require/contract (require/contract-maker te-mode)]
                       [default-ctc
                         (case te-mode
                           ((shallow)
                            #'(procedure-arity-includes/c 1))
                           ((optional)
                            #'any/c)
                           (else
                            #'(or/c struct-predicate-procedure?/c
                                    ((make-any-wrap-warning/c) . c-> . boolean?))))])
           ;; this is needed because this expands to the contract directly without
           ;; going through the normal `make-contract-def-rhs` function.
           (set-box! include-extra-requires? #t)
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
               #,(ignore
                   (with-syntax ((ctc (if (attribute unsafe) ; unsafe- shouldn't generate contracts
                                        #'any/c
                                        #'default-ctc)))
                     #'(define pred-cnt ctc)))
               #,(ignore #`(require/contract pred hidden pred-cnt lib "(-> Any Boolean)")))))]))
    (values (ro-maker deep) (ro-maker shallow) (ro-maker optional))))

(define-values (require-typed-struct-legacy require-typed-struct-legacy-shallow require-typed-struct-legacy-optional
                require-typed-struct require-typed-struct-shallow require-typed-struct-optional)
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

  (define ((rts legacy te-mode) stx)
    (syntax-parse stx #:literals (:)
      [(_ name:opt-parent
          (~optional (~seq (tvar:id ...)) #:defaults ([(tvar 1) '()]))
          (body:typed-field ...)
          (~var input-maker (constructor-term legacy #'name.nm))
          (~optional (~seq #:type-name type:id) #:defaults ([type #'name.nm]))
          unsafe:unsafe-clause
          lib)
       (with-syntax* ([nm #'name.nm]
                      [parent #'name.parent]
                      [hidden (generate-temporary #'name.nm)]
                      [orig-struct-info (generate-temporary #'nm)]
                      [spec (if (syntax-e #'name.parent) #'(nm parent) #'nm)]
                      [(fld ...) #'(body.field ...)]
                      [(ty ...) #'(body.type ...)]
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
                                        (not (bound-identifier=? #'maker-name #'nm))
                                        #'maker-name)]
                      [type (if (stx-null? #'(tvar ...))
                                #'type
                                (untyped-struct-poly #'type))]
                      ;; the type for a polymorphic use of the struct name
                      [poly-type #`(type tvar ...)]
                      ;; the struct type to use for the constructor/selectors
                      [self-type (if (null? (syntax->list #'(tvar ...)))
                                     #'type
                                     #'poly-type)]
                      [r/t/te-mode (case te-mode ((shallow) #'require/typed-shallow) ((optional) #'require/typed-optional) (else #'require/typed))]
                      [r/c/te-mode (require/contract-maker te-mode)])
                     (when (and (not (attribute unsafe.unsafe?))
                                (pair? (syntax->list #'(tvar ...))))
                       (tc-error/stx stx "polymorphic structs are not supported"))

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

                             (void
                               (validate-struct-fields
                                 #'nm (syntax-e #'(fld ...))
                                 (reverse orig-sels)
                                 'require/typed '#,stx))

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
                                  (make-struct-info-wrapper* #'internal-maker si #'type)
                                  si))

                         (dtsi* (tvar ...) spec type (body ...) #:maker maker-name)
                         #,(ignore
                             (with-syntax ((pred-ctc
                                            (case te-mode
                                              ((deep)
                                               #'(or/c struct-predicate-procedure?/c (c-> any-wrap/c boolean?)))
                                              ((shallow)
                                               ;; 2021-12-06 shallow could skip the result checks on a pred if it's a struct-predicate-procedure
                                               #'(procedure-arity-includes/c 1))
                                              (else
                                               #'any/c))))
                               #'(r/c/te-mode pred hidden pred-ctc lib "(-> Any Boolean)")))
                         #,(internal #`(require/typed-internal hidden (Any -> Boolean :
                                                                           #,(if (stx-null? #'(tvar ...))
                                                                                 #'type
                                                                                 #`(type #,@(stx-map (const (syntax Any)) #'(tvar ...)))))))
                         (r/t/te-mode #:internal (maker-name real-maker) type lib
                                        #:struct-maker parent
                                        #,@(if (attribute unsafe.unsafe?) #'(unsafe-kw) #'()))

                         ;This needs to be a different identifier to meet the specifications
                         ;of struct (the id constructor shouldn't expand to it)
                         #,(if (syntax-e #'extra-maker)
                               #`(r/t/te-mode #:internal (maker-name extra-maker) type lib
                                                #:struct-maker parent
                                                #,@(if (attribute unsafe.unsafe?) #'(unsafe-kw) #'()))
                               #'(begin))

                         #,@(if (attribute unsafe.unsafe?)
                                #'((r/t/te-mode #:internal sel (All (tvar ...) (self-type -> ty)) lib unsafe-kw) ...)
                                #'((r/t/te-mode lib [sel (All (tvar ...) (self-type -> ty))]) ...)))))]))

  (values (rts #t deep) (rts #t shallow) (rts #t optional)
          (rts #f deep) (rts #f shallow) (rts #f optional))))

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
