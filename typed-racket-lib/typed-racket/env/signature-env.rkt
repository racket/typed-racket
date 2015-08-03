#lang racket/base

;; Environment for signature definitions
;; to track bindings and type definitions inside of signatures

(provide register-signature!
         lookup-signature
         signature-env-map)

(require syntax/id-table
         (for-syntax syntax/parse racket/base)
         "../utils/utils.rkt"
         (utils tc-utils))

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
    (parameterize ([siganture-env new-env]) . b)))

;; lookup-signature : identifier? -> (or/c #f Signature?)
;; look up the signature corresponding to the given identifier
;; in the signature environment
(define (lookup-signature id)
  (free-id-table-ref (signature-env) id #f))

(define (signature-env-map f)
  (free-id-table-map (signature-env) f))


(provide print-signature-env)
(define (print-signature-env)
  (printf "Printing Signature Env:\n")
  (free-id-table-for-each
   (signature-env)
   (lambda (id sig)
     (printf "id: ~a\n" id)
     (printf "sig: ~a\n" sig)))
  (printf "End Print Signature Env\n\n"))
