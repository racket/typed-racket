#lang racket/base

;; This file implements unit signatures for typed racket

(provide define-signature)

(require "../utils/utils.rkt"
         "colon.rkt"
         (for-syntax syntax/parse
                     racket/base
                     racket/list
                     racket/syntax
                     syntax/kerncase
                     "../private/syntax-properties.rkt"
                     "../typecheck/internal-forms.rkt"
                     syntax/id-table
                     racket/dict
                     racket/unit-exptime
                     (utils tc-utils))
         (only-in racket/unit
                  [define-signature untyped-define-signature]
                  extends)
         (for-label "colon.rkt")
         (submod "../typecheck/internal-forms.rkt" forms)
         (only-in "../../typed/racket/base.rkt" define-type)
         (for-template (rep type-rep)))

(begin-for-syntax 
  (define-literal-set colon #:for-label (:))
  
  ;; TODO: there should be a more extensible way of handling signatures
  (define-syntax-class signature-forms
    (pattern (form:def-sig-form ...)))
  
  (define-syntax-class def-sig-form
    #:attributes (internal-form erased)
    (pattern :sig-var-form
             #:attr kind 'var)
    
    (pattern :sig-type-form
             #:attr kind 'type))
  
  (define-syntax-class sig-var-form
    #:literal-sets (colon)
    (pattern [name:id : type]
             #:with internal-form #'(name type)
             #:with erased #'name))

  ;; Preliminary support for type definitions in signatures
  ;; The form is allowed in signature definitions, but currently
  ;; is ignored
  (define-syntax-class sig-type-form
    #:literals (define-type)
    (pattern (define-type t ty)
             #:with internal-form #'(t ty)
             ;; FIXME: the right thing to do for untyped code
             ;; might be to add it as syntax into the signature 
             ;; so that the types will error on use in untyped code?
             #:with erased #'t)) ;; WRONG WRONG
  
  (define-splicing-syntax-class extends-form
    #:literals (extends)
    (pattern (~seq extends super:id)
             #:with internal-form #'super
             #:with extends-id #'super
             #:attr form #'(extends super))
    (pattern (~seq)
             #:with internal-form #'#f
             #:with extends-id '()
             #:attr form '())))



(define-for-syntax (process-signature-forms forms)
  (define-values (members-raw aliases-raw) 
    (for/fold ([members-raw null]
               [aliases-raw null])
              ([form (in-list forms)])
      (syntax-parse form
        [alias:sig-type-form (values members-raw null)]
        [member:sig-var-form
         (values (cons (syntax-e #'member.internal-form) members-raw) null)])))
  (values (reverse members-raw) (reverse aliases-raw)))


;; typed define-signature macro
(define-syntax (define-signature stx)
  (syntax-parse stx
    [(_ sig-name:id super-form:extends-form forms:signature-forms)
     (define-values (members aliases)
       (process-signature-forms (syntax->list #'forms)))
     (define erased-members (map car members))
     #`(begin
         #,(ignore (quasisyntax/loc stx
                     (untyped-define-signature sig-name #,@(attribute super-form.form)
                                               (#,@erased-members))))
         #,(internal (quasisyntax/loc stx
                       (define-signature-internal sig-name super-form.internal-form
                         (#,@members)
                         ;; no need to further check parent information
                         #f))))]))
