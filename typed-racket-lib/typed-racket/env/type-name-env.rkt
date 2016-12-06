#lang racket/base

;; Environment for type names

(require "../utils/utils.rkt"
         "env-utils.rkt")

(require syntax/id-table
         (contract-req)
         (env type-alias-env)
         (utils tc-utils)
         (rep type-rep free-variance)
         (types utils))

(provide/cond-contract [register-type-name
                        (->* (identifier?) (Type?) any)]
                       [register-type-names
                        (-> (listof identifier?) (listof Type?) any)]
                       [add-alias (-> identifier? identifier? any)]
                       [type-name-env-map
                        (-> (-> identifier? (or/c #t Type?) any) any)]
                       [type-variance-env-map
                        (-> (-> identifier? (listof variance?) any) any)]
                       [type-name-env-for-each
                        (-> (-> identifier? (or/c #t Type?) any) void?)]
                       [type-variance-env-for-each
                        (-> (-> identifier? (listof variance?) any) void?)]
                       [lookup-type-name
                        (->* (identifier?) (procedure?) (or/c #t Type?))]
                       [register-type-variance!
                        (-> identifier? (listof variance?) any)]
                       [lookup-type-variance
                        (-> identifier? (listof variance?))]
                       [add-constant-variance!
                        (-> identifier? (or/c #f (listof identifier?)) any)]
                       [refine-variance!
                        (-> (listof identifier?)
                            (listof Type?)
                            (listof (or/c #f (listof symbol?)))
                            any)])

;; a mapping from id -> type (where id is the name of the type)
(define the-mapping
  (make-free-id-table))

;; add a name to the mapping
(define (register-type-name id [type #t])
  (free-id-table-set! the-mapping id type))

;; add a bunch of names to the mapping
(define (register-type-names ids types)
  (for-each register-type-name ids types))

;; given an identifier, return the type associated with it
;; optional argument is failure continuation - default calls lookup-fail
;; identifier (-> error) -> type
(define (lookup-type-name id [k (lambda () (lookup-type-fail id))])
  (begin0
    (free-id-table-ref the-mapping id k)
    (add-disappeared-use id)))


;; map over the-mapping, producing a list
;; (id type -> T) -> listof[T]
(define (type-name-env-map f)
  (sorted-dict-map the-mapping f id<))

(define (type-name-env-for-each f)
  (sorted-dict-for-each the-mapping f id<))

(define (add-alias from to)
  (when (lookup-type-name to (lambda () #f))
    (register-resolved-type-alias
     from
     (make-Name to 0 #t))))


;; a mapping from id -> listof[Variance] (where id is the name of the type)
(define variance-mapping
  (make-free-id-table))

;; add a name to the mapping
(define (register-type-variance! id variance)
  (free-id-table-set! variance-mapping id variance))

(define (lookup-type-variance id)
  (free-id-table-ref
   variance-mapping id
   (lambda () (lookup-variance-fail id))))

;; map over the-mapping, producing a list
;; (id variance -> T) -> listof[T]
(define (type-variance-env-map f)
  (sorted-dict-map variance-mapping f id<))

(define (type-variance-env-for-each f)
  (sorted-dict-for-each variance-mapping f id<))

;; Refines the variance of a type in the name environment
(define (refine-variance! names types tvarss)
  (let loop ()
    (define sames?
      (for/and ([name (in-list names)]
                [type (in-list types)]
                [tvars (in-list tvarss)])
        (cond
          [(or (not tvars) (null? tvars)) #t]
          [else
            (define free-vars (free-vars-hash (free-vars* type)))
            (define variance (map (λ (v) (hash-ref free-vars v variance:const)) tvars))
            (define old-variance (lookup-type-variance name))

            (register-type-variance! name variance)
            (equal? variance old-variance)])))
    (unless sames? (loop))))

;; Initialize variance of the given id to Constant for all type vars
(define (add-constant-variance! name vars)
  (unless (or (not vars) (null? vars))
    (register-type-variance! name (map (lambda (_) variance:const) vars))))

