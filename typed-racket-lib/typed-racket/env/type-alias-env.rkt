#lang racket/base

(require "../utils/utils.rkt"
         "env-utils.rkt"
         syntax/id-table racket/dict
         (utils tc-utils)
         (typecheck renamer)
         racket/match)

(provide register-type-alias
         lookup-type-alias
         resolve-type-aliases
         register-resolved-type-alias
         type-alias-env-map
         type-alias-env-for-each)

(define-struct alias-def () #:inspector #f)
(define-struct (unresolved alias-def) (stx [in-process #:mutable]) #:inspector #f)
(define-struct (resolved alias-def) (ty) #:inspector #f)

;; a mapping from id -> alias-def (where id is the name of the type)
(define the-mapping
  (make-free-id-table))

(define (mapping-put! id v) (free-id-table-set! the-mapping id v))

;(trace mapping-put!)

;; add a name to the mapping
;; identifier type-stx -> void
(define (register-type-alias id stx)
  ;(printf "registering type ~a\n~a\n" (syntax-e id) id)
  (mapping-put! id (make-unresolved stx #f)))

(define (register-resolved-type-alias id ty)
  (mapping-put! id (make-resolved ty)))

(define (lookup-type-alias id parse-type [k (lambda () (tc-error "Unknown type alias: ~a" (syntax-e id)))])
  (match (or (free-id-table-ref the-mapping id #f)
             (free-id-table-ref the-mapping (un-rename id) #f))
    [#f (k)]
    [(struct unresolved (stx #f))
     (resolve-type-alias id parse-type)]
    [(struct unresolved (stx #t))
     (tc-error/stx stx "Recursive Type Alias Reference")]
    [(struct resolved (t)) t]))

(define (resolve-type-alias id parse-type)
  (define v (free-id-table-ref the-mapping id))
  (match v
    [(struct unresolved (stx _))
     (set-unresolved-in-process! v #t)
     (let ([t (parse-type stx)])
       (mapping-put! id (make-resolved t))
       t)]
    [(struct resolved (t))
     t]))

(define (resolve-type-aliases parse-type)
  (for ([id (in-dict-keys the-mapping)])
    (resolve-type-alias id parse-type)))

;; map over the-mapping, producing a list
;; (id type -> T) -> listof[T]
(define (type-alias-env-map f)  
  (for/list ([(id t) (in-sorted-dict the-mapping id<)]
             #:when (resolved? t))
    (f id (resolved-ty t))))

(define (type-alias-env-for-each f)  
  (for ([(id t) (in-sorted-dict the-mapping id<)]
        #:when (resolved? t))
    (f id (resolved-ty t))))
