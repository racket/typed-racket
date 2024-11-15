#lang racket/base

(require racket/struct-info
         typed-racket/utils/tc-utils)

(provide make-typed-renaming un-rename)

;; a constructor for typed renamings that attach the required
;; 'not-free-identifier properties
(define (make-typed-renaming deep-id untyped-id shallow-id optional-id)
  (typed-renaming (syntax-property deep-id 'not-free-identifier=? #t)
                  (syntax-property untyped-id 'not-free-identifier=? #t)
                  (syntax-property shallow-id 'not-free-identifier=? #t)
                  (syntax-property optional-id 'not-free-identifier=? #t)))

;; deep-id : identifier
;; untyped-id : identifier
;; shallow-id : identifier
;; optional-id : identifier
(struct typed-renaming (deep-id untyped-id shallow-id optional-id)
  ;; prevent the rename transformer from expanding in
  ;; module-begin context because the typed context flag
  ;; will not be set until the module-begin
  #:property prop:expansion-contexts
  '(expression top-level module definition-context)
  ;; delay the rename transformer target selection until
  ;; expansion time when the typed context flag is set correctly
  #:property prop:rename-transformer
  (λ (obj)
    (if (unbox typed-context?)
      (case (current-type-enforcement-mode)
        ((shallow)
         (typed-renaming-shallow-id obj))
        ((optional)
         (typed-renaming-optional-id obj))
        (else ;;(deep #f)
         (typed-renaming-deep-id obj)))
      (typed-renaming-untyped-id obj))))

;; Undo renaming for type lookup.
;; Used because of macros that mark the identifier used as the binding such as
;; kw-application or struct constructors
;;
;; The syntax-transforming check is for unit tests
(define (un-rename id)
  (cond
    [(syntax-transforming?)
     (define-values (binding new-id) (syntax-local-value/immediate id (lambda () (values #f #f))))
     (if (typed-renaming? binding) new-id id)]
    [else id]))
