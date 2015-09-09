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
                     (typecheck internal-forms)
                     syntax/id-table
                     racket/dict
                     racket/unit-exptime
                     (utils tc-utils))
         (only-in racket/unit
                  [define-signature untyped-define-signature]
                  extends)
         (for-label "colon.rkt")
         (submod "../typecheck/internal-forms.rkt" forms)
         (only-in "../../typed/racket/base.rkt" define-type))

(begin-for-syntax 
  (define-literal-set colon #:for-label (:))
  
  ;; TODO: there should be a more extensible way of handling signatures
  (define-syntax-class signature-forms
    (pattern (form:def-sig-form ...)))
  
  (define-syntax-class def-sig-form
    #:attributes (internal-form erased)
    (pattern :sig-var-form
             #:attr kind 'var)
    ;; The define-type form is explicitly disallowed until I can figure out how
    ;; to sensibly support them in signature definitions - dfeltey
    (pattern :sig-type-form
             #:fail-when #t "type definitions are not allowed within signature definitions"
             #:attr kind 'type))
  
  (define-syntax-class sig-var-form
    #:literal-sets (colon)
    (pattern [name:id : type]
             #:with internal-form #'(name type)
             #:with erased #'name))

  ;; Preliminary support for type definitions in signatures
  ;; The form is allowed in signature definitions, but currently
  ;; fails on parsing.
  ;; In the future supporting type definitions inside of signatures
  ;; would be a worthwhile feature, but their implemention is not obvious
  (define-syntax-class sig-type-form
    #:literals (define-type)
    (pattern (define-type t ty)
             ;; These attributes are dummy values
             #:attr internal-form #f
             #:attr erased #f))
  
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


;; process-signature-forms : (listof syntax?) -> (listof (pairof id id))
;; Processes the raw syntax of signature forms and returns a list
;; of pairs representing names and types bound by the signature
(define-for-syntax (process-signature-forms forms)
  (for/list ([form (in-list forms)])
    (syntax-parse form
      [member:sig-var-form
       (syntax-e #'member.internal-form)])))


;; typed define-signature macro
;; This expands into the untyped define-signature syntax as well as an
;; internal form used by TR to register signatures in the signature environment
;; The `define-signature-internal` form specifies
;; - the `name` of the signature being defined
;; - it's parent-signature, or #f if this signature does not extend another signature
;; - the list of member variables contained in this signature along with their types
;; - and a boolean flag indicating whether the signature came from an instance of
;;   require/typed in which case additional checking must occur when the internal
;;   form is parsed
(define-syntax (define-signature stx)
  (syntax-parse stx
    [(_ sig-name:id super-form:extends-form forms:signature-forms)
     (define members (process-signature-forms (syntax->list #'forms)))
     (define erased-members (map car members))
     #`(begin
         #,(ignore (quasisyntax/loc stx
                     (untyped-define-signature sig-name #,@(attribute super-form.form)
                                               (#,@erased-members))))
         #,(internal (quasisyntax/loc stx
                       (define-signature-internal sig-name
                         #:parent-signature super-form.internal-form
                         (#,@members)
                         ;; no need to further check parent information
                         #:check? #f))))]))
