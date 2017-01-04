#lang racket/base
(require racket/match
         racket/set
         racket/lazy-require) 

;; Ugly hack - should use units
(lazy-require
  ("../env/type-name-env.rkt" (lookup-type-variance)))

(provide 
  ;; Variances
  variance:co variance:contra variance:inv variance:const variance:dotted
  variance? variance->binding

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
    (define-struct Variance () #:transparent)
    (define-struct (Covariant Variance) () #:transparent)
    (define-struct (Contravariant Variance) () #:transparent)
    (define-struct (Invariant Variance) () #:transparent)
    (define-struct (Constant Variance) () #:transparent)
    ;; not really a variance, but is disjoint with the others
    (define-struct (Dotted Variance) () #:transparent)
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
(struct combined-frees (table computed) #:transparent)
(struct app-frees (name args) #:transparent)
(struct remove-frees (inner name) #:transparent)


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
      (match frees
        [(combined-frees new-hash new-computed)
         (values (combine-hashes (list hash new-hash))
                 (append new-computed computed))])))
  (combined-frees hash computed))


(define (free-vars-remove frees name)
  (match frees
    [(combined-frees hash computed)
     (combined-frees (hash-remove hash name)
                     (map (λ (v) (remove-frees v name)) computed))]))

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
     (combine-hashes
      (for/list ((var (lookup-type-variance name)) (arg args))
        (free-vars-hash
         (match var
           [(? variance:co?) arg]
           [(? variance:contra?) (flip-variances arg)]
           [(? variance:inv?) (make-invariant arg)]
           [(? variance:const?) (make-constant arg)]))))]))


;; frees = HT[Idx,Variance] where Idx is either Symbol or Number
;; (listof frees) -> frees
(define (combine-hashes hashes)
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


