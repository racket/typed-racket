#lang racket/base

;; This module provides helper functions for typed signatures

(require "../utils/utils.rkt"
         syntax/id-set
         (utils tc-utils)
         (env signature-env)
         (rep type-rep)
         (private parse-type)
         syntax/parse
         racket/list
         racket/match
         racket/promise
         racket/unit-exptime
         (for-template racket/base
                       (submod "../typecheck/internal-forms.rkt" forms)))

(provide parse-and-register-signature! signature->bindings signatures->bindings)

;; parse-signature : Syntax -> Signature
;; parses the internal syntax of a signature form to build the internal representation
;; of a typed signature and registers the internal representation with the
;; Signature environment
;; The parsed syntax is created by uses of `define-signature-internal` which are
;; inserted by the typed version of the `define-signature` macro, this function
;; uses that syntax to created a Typed Representation of the signature to
;; place in the Signature environment
;; The parsed syntax contains the following fields:
;; - name is the name of the signature being defined
;; - the parent-signature is the name of the signature being extended or #f
;;   if the signature defintion does not extend any signature
;; - a list of bindings, each of the form [name : Type], for each variable
;;   in the signature
;; - The check field indicates that this syntax came from a use of require/typed
;;   using the #:signature clause, and listed bindings muct be checked to ensure
;;   they are consistent with the statically known signature variables
(define (parse-and-register-signature! form)
  (syntax-parse form
    #:literal-sets (kernel-literals)
    #:literals (values define-signature-internal)
    [(define-values ()
       (begin
         (quote-syntax
          (define-signature-internal name
            #:parent-signature super
            (binding ...)
            #:check? check) #:local)
              (#%plain-app values)))
     (define raw-map (syntax->list #'(binding ...)))
     (define check? (syntax->datum #'check))
     (define extends (get-extended-signature #'name #'super check? form))
     (define super-bindings (get-signature-mapping extends))
     (define new-bindings (map parse-signature-binding raw-map))
     (define pre-mapping (append super-bindings new-bindings))

     ;; Make sure a require/typed signature has bindings listed
     ;; that are consistent with its statically determined bindings
     (when check?
       (check-signature-bindings #'name (map car pre-mapping) form))

     ;; require/typed signature bindings may not be in the correct order
     ;; this fixes the ordering based on the static order determined
     ;; by signature-members
     (define mapping (if check?
                         (fix-order #'name pre-mapping)
                         pre-mapping))
     (define signature (make-Signature #'name extends mapping))
     (register-signature! #'name signature)]))

;; check-signature-bindings : Identifier (Listof Identifier) -> Void
;; checks that the bindings of a signature identifier are consistent with
;; those listed in a require/typed clause
(define (check-signature-bindings name vars stx)
  (match-define-values (_ inferred-vars inferred-defs _) (signature-members name name))
  (define (make-id-set set) (immutable-free-id-set set #:phase (add1 (syntax-local-phase-level))))
  (define inferred-vars-set (make-id-set inferred-vars))
  (define vars-set (make-id-set vars))
  (unless (empty? inferred-defs)
    (tc-error/stx name "untyped signatures containing definitions are prohibited"))
  (unless (free-id-set=? inferred-vars-set vars-set)
    (tc-error/fields "required signature declares inconsistent members"
                     "expected members" (map syntax-e inferred-vars)
                     "received members" (map syntax-e vars)
                     #:stx stx)))

;; get-extended-signature : Identifier Syntax Boolean -> (Option Signature)
;; Checks if the extended signature information must be inferred and looks
;; up the super signature in the environment
;; Raises an error if a super signature is inferred that is not in the
;; signature environment indicative of a signature that should be require/typed
;; but was not, a typed binding for the parent signature is necessary to correctly
;; check subtyping for units
(define (get-extended-signature name super check? stx)
  (cond
   [check?
    (match-define-values (inferred-super _ _ _) (signature-members name name))
    (and inferred-super
         (or (and (lookup-signature inferred-super) inferred-super)
             (tc-error/fields "required signature extends an untyped signature"
                              "required signature" (syntax-e name)
                              "extended signature" (syntax-e inferred-super)
                              #:stx stx)))]
   [(not (syntax->datum super)) #f]
   ;; This case should probably be an error, because if the signature was not false
   ;; the lookup may still silently fail which should not be allowed here
   [else (or (and (lookup-signature super) super)
             (tc-error/fields "signature definition extends untyped signature"
                              "in the definition of signature" (syntax-e name)
                              "which extends signature" (syntax-e super)
                              #:stx stx))]))

;; parse-signature-binding : Syntax -> (list/c identifier? syntax?)
;; parses the binding forms inside of a define signature into the 
;; form used by the Signature type representation
;; The call to `parse-type` is delayed to allow signatures and type aliases
;; to be mutually recursive, after aliases are registered in the environment
;; the promise will be forced to perform the actual type parsing
(define (parse-signature-binding binding-stx)
  (syntax-parse binding-stx
    [[name:id type]
     (cons #'name (delay (parse-type #'type)))]))

;; signature->bindings : identifier? -> (listof (cons/c identifier? type?))
;; GIVEN: a signature name
;; RETURNS: the list of variables bound by that signature
;;          inherited bindings come first
(define (signature->bindings sig-id)
  (define sig (lookup-signature sig-id))
  (let loop ([sig (Signature-extends sig)]
             [mapping (Signature-mapping sig)]
             [bindings null])
    (if sig
        (loop (Signature-extends (lookup-signature sig))
              (Signature-mapping (lookup-signature sig))
              (append mapping bindings))
        (append mapping bindings))))

;; (listof identifier?) -> (listof (cons/c identifier? type?))
;; GIVEN: a list of signature names
;; RETURNS: the list of all bindings from those signatures
;; TODO: handle required renamings/prefix/only/except
(define (signatures->bindings ids)
  (append-map signature->bindings ids))

;; get-signature-mapping : (Option Signature) -> (Listof (Cons Id Type))
(define (get-signature-mapping sig)
  (if sig (Signature-mapping (lookup-signature sig)) null))

;; fix-order : id (listof (cons/c id type?)) -> (listof (cons/c id type?)
;; Returns a reordered list of signature bindings to match the order given
;; by signature-members
(define (fix-order sig-id sig-bindings)
  (match-define-values (_ members _ _) (signature-members sig-id sig-id))
  (map
   (lambda (id) (assoc id sig-bindings free-transformer-identifier=?))
   members))
