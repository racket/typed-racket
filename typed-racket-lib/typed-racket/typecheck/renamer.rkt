#lang racket/base

(require typed-racket/utils/tc-utils)

(provide typed-renaming un-rename)

;; target : identifier
;; alternate : identifier
(define-struct typed-renaming (target alternate)
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
        (typed-renaming-target obj)
        (typed-renaming-alternate obj))))

;; Undo renaming for type lookup.
;; Used because of macros that mark the identifier used as the binding such as
;; kw-application or struct constructors
;;
;; The syntax-transforming check is for unit tests
(define (un-rename id)
  (if (syntax-transforming?)
      (let-values (((binding new-id) (syntax-local-value/immediate id (lambda () (values #f #f)))))
        (if (typed-renaming? binding)
            new-id
            id))
      id))
