#lang racket/base

;; Environment for signature definitions
;; to track bindings and type definitions inside of signatures

(provide register-signature!
         finalize-signatures!
         lookup-signature
         lookup-signature/check
         signature-env-map
         signature-env-for-each)

(require (for-syntax racket/base
                     syntax/parse)
         racket/promise
         syntax/private/id-table
         "../rep/type-rep.rkt"
         "../utils/tc-utils.rkt"
         "env-utils.rkt")

;; initial signature environment
(define signature-env (make-free-id-table))

;; register-signature! : identifier? Signature? -> Void
;; adds a mapping from the given identifier to the given signature
;; in the signature environment
(define (register-signature! id sig)
  (when (lookup-signature id)
    (tc-error/fields "duplicate signature definition"
                     "identifier" (syntax-e id)))
  (free-id-table-set! signature-env id sig))

;; Iterate over the signature environment forcing the types of bindings
;; in each signature
(define (finalize-signatures!)
  (sorted-free-id-table-for-each signature-env (λ (id sig) (force sig))))

;; lookup-signature : identifier? -> (or/c #f Signature?)
;; look up the signature corresponding to the given identifier
;; in the signature environment
(define (lookup-signature id)
  (cond
    [(free-id-table-ref signature-env id #f) => force]
    [else #f]))

;; lookup-signature/check : identifier? -> Signature?
;; lookup the identifier in the signature environment
;; errors if there is no such typed signature
(define (lookup-signature/check id)
  (or (lookup-signature id)
      (tc-error/fields "use of untyped signature in typed code"
                       #:more "consider using `require/typed' to import it"
                       "signature" (syntax-e id)
                       #:stx id)))

(define (signature-env-map f)
  (sorted-free-id-table-map signature-env (λ (id sig) (f id (force sig)))))

(define (signature-env-for-each f)
  (sorted-free-id-table-for-each signature-env (λ (id sig) (f id (force sig)))))
