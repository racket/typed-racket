#lang racket/base
(require racket/match
         racket/set
         racket/list
         "../rep/type-constr.rkt"
         "../utils/utils.rkt"
         racket/lazy-require
         "../private/user-defined-type-constr.rkt"
         "../env/type-constr-env.rkt")

(provide
  ;; Variances
  variance:co variance:contra variance:inv variance:const variance:dotted
  variance? variance->binding
  refine-user-defined-constructor-variances!
  build-default-variances

  ;; Construcing frees
  combine-frees flip-variances
  make-invariant make-constant 
  instantiate-frees
  empty-free-vars
  single-free-var
  free-vars-remove

  ;; Examining frees
  free-vars-hash
  free-vars-names
  free-vars-has-key?
  variance:co?
  variance:contra?
  variance:inv?
  variance:const?
  variance:dotted?)


;; this file contains support for calculating the free variables/indexes of types
;; actual computation is done in rep-utils.rkt  and type-rep.rkt
(define-values (variance? variance:co
                          variance:contra
                          variance:inv
                          variance:const
                          variance:dotted)
  (let ()
    (define-struct Variance () #:transparent #:authentic)
    (define-struct (Covariant Variance) () #:transparent #:authentic)
    (define-struct (Contravariant Variance) () #:transparent #:authentic)
    (define-struct (Invariant Variance) () #:transparent #:authentic)
    (define-struct (Constant Variance) () #:transparent #:authentic)
    ;; not really a variance, but is disjoint with the others
    (define-struct (Dotted Variance) () #:transparent #:authentic)
    (values Variance? (make-Covariant) (make-Contravariant) (make-Invariant) (make-Constant) (make-Dotted))))

(define (variance:co? x) (eq? x variance:co))
(define (variance:contra? x) (eq? x variance:contra))
(define (variance:inv? x) (eq? x variance:inv))
(define (variance:const? x) (eq? x variance:const))
(define (variance:dotted? x) (eq? x variance:dotted))


(define (variance->binding var)
  (match var
    [(? variance:co?) #'variance:co]
    [(? variance:contra?) #'variance:contra]
    [(? variance:inv?) #'variance:inv]
    [(? variance:const?) #'variance:const]
    [(? variance:dotted?) #'variance:dotted]))

(define (flip-variance v)
  (match v
    [(? variance:co?) variance:contra]
    [(? variance:contra?) variance:co]
    [_ v]))

;;All of these are used internally
;;Only combined-frees is used externally
(struct combined-frees (table computed) #:transparent #:authentic)
(struct app-frees (name args) #:transparent #:authentic)
(struct remove-frees (inner name) #:transparent #:authentic)


;; Base constructors
(define (single-free-var name [variance variance:co])
  (combined-frees (hasheq name variance) null))

(define empty-free-vars 
  (combined-frees (hasheq) null))

;; Computed constructor
(define (instantiate-frees name frees)
  (combined-frees (hasheq) (list (app-frees name frees))))


;; frees -> frees
(define (flip-variances frees)
  (match frees
    [(combined-frees hash computed)
     (combined-frees
      (for/hasheq (((k v) hash))
        (values k (flip-variance v)))
      (map flip-variances computed))]
    [(app-frees name args)
     (app-frees name (map flip-variances args))]
    [(remove-frees inner name)
     (remove-frees (flip-variances inner) name)]))


(define (make-invariant frees)
  (combined-frees
    (for/hasheq ([name (free-vars-names frees)])
      (values name variance:inv))
    null))

(define (make-constant frees)
  (combined-frees
    (for/hasheq ([name (free-vars-names frees)])
      (values name variance:const))
    null))

;; Listof[frees] -> frees
(define (combine-frees freess)
  (define-values (hash computed)
    (for/fold ([hash (hasheq)]
               [computed null])
              ([frees (in-list freess)])
      (match-define (combined-frees new-hash new-computed) frees)
      (values (combine-hashes (list hash new-hash)) (append new-computed computed))))
  (combined-frees hash computed))


(define (free-vars-remove frees name)
  (match-define (combined-frees hash computed) frees)
  (combined-frees (hash-remove hash name) (map (λ (v) (remove-frees v name)) computed)))

;;
(define (free-vars-names vars)
  (match vars
    [(combined-frees hash computed)
     (apply set-union
            (list->seteq (hash-keys hash))
            (map free-vars-names computed))]
    [(remove-frees inner name) (set-remove (free-vars-names inner) name)]
    [(app-frees name args)
     (apply set-union (map free-vars-names args))]))

(define (free-vars-has-key? vars key)
  (set-member? (free-vars-names vars) key))

;; Only valid after full type resolution
(define (free-vars-hash vars)
  (match vars
    [(combined-frees hash computed)
     (combine-hashes (cons hash (map free-vars-hash computed)))]
    [(remove-frees inner name) (hash-remove (free-vars-hash inner) name)]
    [(app-frees name args)
     (debug-print "in app frees ~a ~n" name)
     (combine-hashes
      (for/list ((var (lookup-type-constructor-variances name args))
                 (arg args))
        (define ret
          (free-vars-hash
           (match var
             [(? variance:co?) arg]
             [(? variance:contra?) (flip-variances arg)]
             [(? variance:inv?) (make-invariant arg)]
             [(? variance:const?) (make-constant arg)])))
        (debug-print "ret ~a var ~a name ~a ~n" ret var name)
        ret))]))


;; frees = HT[Idx,Variance] where Idx is either Symbol or Number
;; (listof frees) -> frees
(define (combine-hashes hashes)
  (debug-print "hashes ~a ~n" hashes)
  (define ((combine-var v) w)
    (cond
      [(eq? v w) v]
      [(variance:dotted? v) w]
      [(variance:dotted? w) v]
      [(variance:const? v) w]
      [(variance:const? w) v]
      [else variance:inv]))
    (for*/fold ([ht #hasheq()])
      ([old-free (in-list hashes)]
       [(sym var) (in-hash old-free)])
      (hash-update ht sym (combine-var var) var)))

(define (lookup-type-constructor-variances id args)
  (match (lookup-type-constructor id)
    [(struct* TypeConstructor ([variances variances]))
     variances]
    [_ (build-default-variances args)]))

(define (build-default-variances args)
  (map (lambda _ variance:const) args))


(lazy-require ["./type-rep.rkt" (free-vars*)])

(define (refine-user-defined-constructor-variances! constr-names)
  (let loop ()
    (define sames?
      (for/and ([name (in-list constr-names)])
        (debug-print "name is ~a ~n" name)
        (define constr (lookup-type-constructor name))
        (match-define (struct* TypeConstructor ([real-trep-constr maker]
                                                [variances old-variances]))
          constr)
        (match-define (struct user-defined-type-op [tvars type _ _]) maker)
        (cond
          [(or (not tvars) (null? tvars)) #t]
          [else
           (define vars (free-vars* type))
           (debug-print "~n start ~a ~a ~n" type vars)
           (define var-hash (free-vars-hash vars))
           (define new-variances
             (map (lambda (v)
                    ;; if v does not occur free in the body,
                    ;; then its variance is irrelavent to
                    ;; the type constructor
                    (hash-ref var-hash v variance:const))
                  tvars))
           (set-TypeConstructor-variances! constr new-variances)
           (debug-print "name ~a old-variances ~a new-variances ~a ~n~n" (syntax-e name) old-variances new-variances)
           (equal? old-variances new-variances)])))
    (unless sames? (loop))))
