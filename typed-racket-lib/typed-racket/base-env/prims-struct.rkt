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
                     racket/struct-info
                     "../typecheck/internal-forms.rkt"
                     "annotate-classes.rkt"
                     "../private/parse-classes.rkt"
                     "../private/syntax-properties.rkt"
                     "../typecheck/internal-forms.rkt"))

(begin-for-syntax
  (lazy-require [syntax/struct (build-struct-names)]))

(provide define-typed-struct -struct define-typed-struct/exec dtsi* dtsi/exec*
         define-type-alias define-new-subtype)

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
    #:attributes (guard mutable? transparent? prefab? [prop 1] [prop-val 1])
    (pattern (~seq (~or (~optional (~seq (~and #:mutable mutable?)))
                        (~optional (~seq (~and #:transparent transparent?)))
                        (~optional (~seq (~and #:prefab prefab?)))
                        ;; FIXME: unsound, but relied on in core libraries
                        ;; #:guard ought to be supportable with some work
                        ;; #:property is harder
                        (~optional (~seq #:guard guard:expr))
                        (~seq #:property prop:expr prop-val:expr))
                   ...)))

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
    [(_ nm ((~describe "field specification" [fld:optionally-annotated-name]) ...) [proc : proc-ty])
     (with-syntax*
      ([proc* (with-type* #'proc #'proc-ty)]
       [d-s (ignore-some (syntax/loc stx (define-struct nm (fld.name ...)
                                      #:property prop:procedure proc*)))]
       [dtsi (quasisyntax/loc stx (dtsi/exec* () nm (fld ...) proc-ty))])
      #'(begin d-s dtsi))]))

(define-syntaxes (dtsi* dtsi/exec*)
  (let ()
    (define (mk internal-id)
      (lambda (stx)
        (syntax-parse stx
          [(_ () nm:dtsi-struct-name . rest)
           (internal (quasisyntax/loc stx
                       (#,internal-id
                        #,(struct-info-property #'nm (attribute nm.value)) . rest)))]
          [(_ (vars:id ...) nm:dtsi-struct-name . rest)
           (internal (quasisyntax/loc stx
                       (#,internal-id (vars ...)
                                      #,(struct-info-property #'nm (attribute nm.value)) . rest)))])))
    (values (mk #'define-typed-struct-internal)
            (mk #'define-typed-struct/exec-internal))))



;; User-facing macros for defining typed structure types
(define-syntaxes (define-typed-struct -struct)
  (values
   (lambda (stx)
     (syntax-parse stx
       [(_ vars:maybe-type-vars nm:struct-name (fs:fld-spec ...)
           opts:struct-options)
        (let ([mutable? (if (attribute opts.mutable?) #'(#:mutable) #'())]
              [cname (second (build-struct-names #'nm.name null #t #t))]
              [prefab? (if (attribute opts.prefab?) #'(#:prefab) #'())])
          (with-syntax ([d-s (ignore-some
                               (syntax/loc stx (define-struct nm (fs.fld ...) . opts)))]
                        [dtsi (quasisyntax/loc stx
                                (dtsi* (vars.vars ...) nm (fs.form ...)
                                       #:maker #,cname
                                       #,@mutable?
                                       #,@prefab?))])
            #'(begin d-s dtsi)))]))
   (lambda (stx)
     (syntax-parse stx
       [(_ vars:maybe-type-vars nm:struct-name/new (fs:fld-spec ...)
           opts:struct-options)
        (let ([mutable? (if (attribute opts.mutable?) #'(#:mutable) #'())]
              [prefab? (if (attribute opts.prefab?) #'(#:prefab) #'())])
          (with-syntax ([d-s (ignore (quasisyntax/loc stx
                                       (struct #,@(attribute nm.new-spec) (fs.fld ...)
                                               . opts)))]
                        [dtsi (quasisyntax/loc stx
                                (dtsi* (vars.vars ...)
                                       nm.old-spec (fs.form ...)
                                       #,@mutable?
                                       #,@prefab?))])
            #'(begin d-s dtsi)))]))))


;; this has to live here because it's used below
(define-syntax (define-type-alias stx)
  (define-syntax-class all-vars
    #:literals (All)
    #:attributes (poly-vars)
    (pattern (All (arg:id ...) rest)
             #:with poly-vars #'(arg ...))
    (pattern type:expr #:with poly-vars #'()))

  (define-splicing-syntax-class omit-define-syntaxes
    #:attributes (omit)
    (pattern #:omit-define-syntaxes #:attr omit #t)
    (pattern (~seq) #:attr omit #f))

  (define-splicing-syntax-class type-alias-full
     #:attributes (tname type poly-vars omit)
     (pattern (~seq tname:id (~and type:expr :all-vars) :omit-define-syntaxes))
     (pattern (~seq (tname:id arg:id ...) body:expr :omit-define-syntaxes)
       #:with poly-vars #'(arg ...)
       #:with type (syntax/loc #'body (All (arg ...) body))))

  (syntax-parse stx
    [(_ :type-alias-full)
     (define/with-syntax stx-err-fun
       #'(lambda (stx)
           (raise-syntax-error
            'type-check
            "type name used out of context"
            stx
            (and (stx-pair? stx) (stx-car stx)))))
     #`(begin
         #,(if (not (attribute omit))
               (ignore #'(define-syntax tname stx-err-fun))
               #'(begin))
         #,(internal (syntax/loc stx
                       (define-type-alias-internal tname type poly-vars))))]))

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

