#lang racket/base

(require typed-racket/utils/tc-utils
         (for-template racket/base))

(provide typed-indirection typed-renaming un-rename)

;; FIXME: use `make-variable-like-transformer` to abstract this pattern once
;;        it gets an option to supply a function instead of syntax to operate
;;        on the identifier.
(define (rename obj stx)
  (cond ;; Protect against the case that the renamed identifier is
        ;; the only expression in the module, in which case expansion
        ;; will happen before the #%module-begin of TR sets the typed
        ;; context flag.
        [(eq? (syntax-local-context) 'module-begin)
         #`(#%expression #,stx)]
        [(identifier? stx)
         (if (unbox typed-context?)
             (typed-renaming-target obj)
             (typed-renaming-alternate obj))]
        [else
         (datum->syntax stx
                        (cons (rename obj (car (syntax-e stx)))
                              (cdr (syntax-e stx)))
                        stx
                        stx)]))

;; this sets up a second indirection that just informs TR that there
;; was a renaming established that can be checked by
;; syntax-local-value/immediate
(define-struct rename-indirect (target)
  #:property prop:rename-transformer 0)

(define (typed-indirection id)
  (rename-indirect (syntax-property id 'not-free-identifier=? #t)))

(define-struct typed-renaming (target alternate)
  #:property prop:syntax-local-value
  (λ (obj)
    (if (unbox typed-context?)
        (typed-renaming-target obj)
        (typed-renaming-alternate obj)))
  #:property prop:set!-transformer rename)

;; Undo renaming for type lookup.
;; Used because of macros that mark the identifier used as the binding such as
;; kw-application or struct constructors
;;
;; The syntax-transforming check is for unit tests
(define (un-rename id)
  (if (syntax-transforming?)
      (let-values (((binding new-id) (syntax-local-value/immediate id (lambda () (values #f #f)))))
        (if (rename-indirect? binding)
            new-id
            id))
      id))
