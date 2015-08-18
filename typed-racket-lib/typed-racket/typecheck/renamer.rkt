#lang racket/base

(provide renamer un-rename)

;; target : identifier
;; alternate : identifier
(define-struct typed-renaming (target alternate)
  #:property prop:rename-transformer 0)

(define (renamer id [alt #f])
  (if alt
      (make-typed-renaming (syntax-property id 'not-free-identifier=? #t) alt)
      (make-rename-transformer (syntax-property id 'not-free-identifier=? #t))))

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
