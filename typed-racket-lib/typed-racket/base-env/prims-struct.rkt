#lang racket/base

;; This module defines the forms needed for defining structs in Typed
;; Racket.  The forms here are referenced in and re-provided by
;; "prims.rkt", sometimes under other names.

;; This file is `require`d into all Typed Racket programs, and thus
;; its runtime dependencies should be kept to a minimum. In
;; particular, contracts and `syntax-parse` are both to be avoided as
;; runtime dependencies (syntax time is fine).

(require (submod "../typecheck/internal-forms.rkt" forms)
         "colon.rkt"
         "base-types-extra.rkt"
         "ann-inst.rkt"
         (for-syntax racket/base syntax/parse/pre
                     racket/lazy-require
                     syntax/stx
                     racket/list
                     racket/syntax
                     racket/match
                     racket/struct-info
                     "annotate-classes.rkt"
                     "type-name-error.rkt"
                     "../utils/struct-info.rkt"
                     "../private/parse-classes.rkt"
                     "../private/syntax-properties.rkt"
                     "../typecheck/internal-forms.rkt"))

(begin-for-syntax
  (lazy-require [syntax/struct (build-struct-names)]))

(provide define-typed-struct -struct define-typed-struct/exec dtsi*
         define-type-alias define-new-subtype
         (for-syntax type-name-error))

(define-for-syntax (with-type* expr ty)
  (with-type #`(ann #,expr #,ty)))

;; Syntax classes and helpers for `struct:`
(begin-for-syntax
  (define-syntax-class fld-spec
    #:literals (:)
    #:description "[field-name : type]"
    (pattern [fld:id : ty]
             #:with form this-syntax)
    (pattern fld:id
             #:fail-when #t
             (format "field `~a' requires a type annotation"
                     (syntax-e #'fld))
             #:with form 'dummy))

  (define-syntax-class struct-name
    #:description "struct name (with optional super-struct name)"
    #:attributes (name super)
    (pattern (name:id super:id))
    (pattern name:id
             #:with super #f))

  (define-splicing-syntax-class struct-name/new
    #:description "struct name (with optional super-struct name)"
    (pattern (~seq name:id super:id)
             #:attr old-spec #'(name super)
             #:with new-spec #'(name super))
    (pattern name:id
             #:with super #f
             #:attr old-spec #'name
             #:with new-spec #'(name)))

  (define-splicing-syntax-class maybe-type-vars
   #:description "optional list of type variables"
   #:attributes ((vars 1))
   (pattern (vars:id ...))
   (pattern (~seq) #:attr (vars 1) null))

  (define-splicing-syntax-class struct-options
    #:description "typed structure type options"
    #:attributes (guard mutable? transparent? prefab? cname ecname type untyped
                  [prop 1] [prop-val 1])
    (pattern (~seq (~or (~optional (~seq (~and #:mutable mutable?)))
                        (~optional (~seq (~and #:transparent transparent?)))
                        (~optional (~seq (~and #:prefab prefab?)))
                        (~optional (~or (~and (~seq #:constructor-name cname)
                                              (~bind [ecname #f]))
                                        (~and (~seq #:extra-constructor-name ecname)
                                              (~bind [cname #f]))))
                        (~optional (~seq #:type-name type:id))
                        ;; FIXME: unsound, but relied on in core libraries
                        ;; #:guard ought to be supportable with some work
                        ;; #:property is harder
                        (~optional (~seq #:guard guard:expr))
                        (~seq #:property prop:expr prop-val:expr))
                   ...)
             #:attr untyped #`(#,@(if (attribute mutable?) #'(#:mutable) #'())
                               #,@(if (attribute transparent?) #'(#:transparent) #'())
                               #,@(if (attribute prefab?) #'(#:prefab) #'())
                               #,@(if (attribute cname) #'(#:constructor-name cname) #'())
                               #,@(if (attribute ecname) #'(#:extra-constructor-name ecname) #'())
                               #,@(if (attribute guard) #'(#:guard guard) #'())
                               #,@(append* (for/list ([prop (in-list (attribute prop))]
                                                      [prop-val (in-list (attribute prop-val))])
                                             (list #'#:property prop prop-val))))))

  (define-syntax-class dtsi-struct-name
    #:description "struct name (with optional super-struct name)"
    #:attributes (name super value)
    (pattern ((~var name (static struct-info? "struct name")) super:id)
             #:attr value (attribute name.value))
    (pattern (~var name (static struct-info? "struct name"))
             #:attr value (attribute name.value)
             #:with super #f)))

(define-syntax (define-typed-struct/exec stx)
  (syntax-parse stx #:literals (:)
    [(_ nm:struct-name ((~describe "field specification" [fld:optionally-annotated-name]) ...)
        [proc : proc-ty] (~optional (~seq #:type-name type:id)))
     (with-syntax* ([type (or (attribute type) #'nm.name)]
                    [proc* (with-type* #'proc #'proc-ty)])
       #'(define-typed-struct nm (fld ...) #:type-name type #:property prop:procedure proc*))]))

(define-for-syntax (mk-maybe-type-name-def stx name type-name si sname-is-constr?)
  (cond
    [(not (free-identifier=? name type-name))
     (define/with-syntax si-stx (struct-info->syntax si))
     (quasisyntax/loc stx
       (define-syntax #,type-name
         (make-struct-info-wrapper* (syntax #,name) si-stx (syntax #,type-name)
                                    #,sname-is-constr?)))]
    [else
     #'(begin)]))

(define-syntax (dtsi* stx)
  (syntax-parse stx
    [(_ (vars:id ...) nm:dtsi-struct-name type-name:id (fld ...) rst:dtsi-fields)
     (define def
       (quasisyntax/loc stx
         (define-typed-struct-internal
           (vars ...)
           #,(struct-info-property #'nm (attribute nm.value)) type-name (fld ...) . rst)))
     ;; now we can use the structure's name to get the struct's *struct
     ;; info* when the type name != the struct name, we create a special
     ;; transfomer binding so the type name can be also used as the struct id
     (define/with-syntax maybe-type-name-def
       (mk-maybe-type-name-def stx #'nm.name #'type-name (attribute nm.value)
                               ;; the structure's type and name can be used
                               ;; as its constructors, if
                               ;; #:extra-constructor-name is set,
                               ;; #:constructor-name is set or it is the same as
                               ;; the structure name
                               (and (or (attribute rst.extra-maker)
                                        (not (attribute rst.maker))
                                        (free-identifier=? (attribute rst.maker) #'nm.name))
                                    #t)))

     #`(begin #,(internal def)
              maybe-type-name-def)]))


;; User-facing macros for defining typed structure types
(define-syntax (define-typed-struct stx)
  (syntax-parse stx
    [(_ vars:maybe-type-vars nm:struct-name (fs:fld-spec ...) opts:struct-options)
     (quasisyntax/loc stx
       (-struct #,@#'vars
                #,@(if (stx-pair? #'nm)
                       #'nm
                       (list #'nm))
                (fs ...)
                ;; If there's already a (extra) constructor name supplied,
                ;; then Racket's `define-struct` doesn't define a `make-`
                ;; constructor either so don't pass anything extra.
                #,@(if (or (attribute opts.cname)
                           (attribute opts.ecname))
                       null
                       (list #'#:extra-constructor-name
                             (second (build-struct-names #'nm.name null #t #t))))
                . opts))]))

(define-syntax (-struct stx)
  (syntax-parse stx
    [(_ vars:maybe-type-vars nm:struct-name/new (fs:fld-spec ...)
        opts:struct-options)
     (let ([mutable? (if (attribute opts.mutable?) #'(#:mutable) #'())]
           [prefab? (if (attribute opts.prefab?) #'(#:prefab) #'())]
           [maker (if (attribute opts.cname)
                      #`(#:maker #,(attribute opts.cname))
                      #'())]
           [extra-maker (if (attribute opts.ecname)
                            #`(#:extra-maker #,(attribute opts.ecname))
                            #'())]
           [properties (if (not (empty? (attribute opts.prop)))
                           #`(#,@(append* (for/list ([prop (in-list (attribute opts.prop))])
                                            (list #'#:property prop))))
                           #'())])
       (with-syntax* ([type (or (attribute opts.type) #'nm.name)]
                      [d-s (struct-type-property (ignore (quasisyntax/loc stx
                                                           (struct #,@(attribute nm.new-spec) (fs.fld ...)
                                                             . opts.untyped)))
                                                 #'type)]
                      [prop-vals (quasisyntax/loc stx
                                   (define prop-val-li (list #,@(attribute opts.prop-val))))]
                      [dtsi (quasisyntax/loc stx
                              (dtsi* (vars.vars ...)
                                     nm.old-spec type (fs.form ...)
                                     #,@mutable?
                                     #,@prefab?
                                     #,@maker
                                     #,@extra-maker
                                     #,@properties
                                     ))])
         #'(begin d-s dtsi)))]))

;; this has to live here because it's used below
(define-syntax (define-type-alias stx)
  (define-splicing-syntax-class omit-define-syntaxes
    #:attributes (omit)
    (pattern #:omit-define-syntaxes #:attr omit #t)
    (pattern (~seq) #:attr omit #f))

  (define-splicing-syntax-class type-abbrev
     #:attributes (tname body omit params)
    (pattern (~seq tname:id (~and body:expr) :omit-define-syntaxes)
             #:with params #'())
    (pattern (~seq (tname:id arg:id ...) body:expr :omit-define-syntaxes)
             #:with params #'(arg ...)))


  (syntax-parse stx
    [(_ :type-abbrev)
     #`(begin
         #,(if (not (attribute omit))
               (ignore (syntax/loc stx (define-syntax tname type-name-error)))
               #'(begin))
         #,(internal (syntax/loc stx
                       (define-type-alias-internal tname body params))))]))

(define-syntax define-new-subtype
  (lambda (stx)
    (unless (memq (syntax-local-context) '(module module-begin))
      (raise-syntax-error 'define-new-subtype
                          "can only be used at module top-level"))
    (syntax-parse stx
      [(define-new-subtype ty:id (constructor:id rep-ty:expr))
       #:with gen-id (generate-temporary #'ty)
       #`(begin
           (define-type-alias ty (Distinction ty gen-id rep-ty))
           #,(ignore
              #'(define constructor (lambda (x) x)))
           #,(internal (syntax/loc stx
                         (define-new-subtype-internal ty (constructor rep-ty) #:gen-id gen-id))))])))
