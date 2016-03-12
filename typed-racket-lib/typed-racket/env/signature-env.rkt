#lang racket/base

;; Environment for signature definitions
;; to track bindings and type definitions inside of signatures

(provide register-signature!
         finalize-signatures!
         lookup-signature
         lookup-signature/check
         signature-env-map
         with-signature-env/extend)

(require syntax/id-table
         racket/match
         racket/promise
         (for-syntax syntax/parse racket/base)
         "env-utils.rkt"
         "../utils/utils.rkt"
         (utils tc-utils)
         (rep type-rep))

;; initial signature environment
(define signature-env (make-parameter (make-immutable-free-id-table)))

;; register-signature! : identifier? Signature? -> Void
;; adds a mapping from the given identifier to the given signature
;; in the signature environment
(define (register-signature! id sig)
  (when (lookup-signature id)
    (tc-error/fields "duplicate signature definition"
                     "identifier" (syntax-e id)))
  (signature-env (free-id-table-set (signature-env) id sig)))


(define-syntax-rule (with-signature-env/extend ids sigs . b)
  (let ([ids* ids]
        [sigs* sigs])
    (define new-env 
      (for/fold ([env (signature-env)])
                ([id (in-list ids*)]
                 [sig (in-list sigs*)])
        (free-id-table-set env id sig)))
    (parameterize ([signature-env new-env]) . b)))

;; Iterate over the signature environment forcing the types of bindings
;; in each signature
(define (finalize-signatures!)
  (signature-env
   (make-immutable-free-id-table
    (signature-env-map
     (lambda (id sig)
       (cons
        id
        (match sig
          [(Signature: name extends mapping)
           (make-Signature
            name
            extends
            (map
             (match-lambda [(cons id ty) (cons id (force ty))])
             mapping))]
          [_ #f])))))))

;; lookup-signature : identifier? -> (or/c #f Signature?)
;; look up the signature corresponding to the given identifier
;; in the signature environment
(define (lookup-signature id)
  (free-id-table-ref (signature-env) id #f))

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
  (sorted-dict-map (signature-env) f id<))
