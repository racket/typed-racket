#lang racket/base

;; Manages the restrictions on what kind of contract is viable.
;; Some combinators cannot support arbitrary contracts. Ex: hash/c needs a flat contract on the key.
;; This module provides the functions for manipulating a set of such constraints.
;;
;; Constructors:
;;   simple-contract-restrict: kind? -> contract-restrict?
;;     This means that the generated contract will be contract of the supplied kind.
;;
;;   variable-contract-restrict: identifier? -> contract-restrict?
;;     This means that the generated contract will be of the same kind as the recursive contract
;;     referenced by the variable.
;;
;;   merge-restricts: kind? contract-restrict? ... -> contract-restrict?
;;   merge-restricts*: kind? (listof contract-restrict?) -> contract-restrict?
;;     This means that the generated contract will be the max of kind and all of the other contract
;;     restricts.
;;
;;   add-constraint: contract-restrict? kind? -> contract-restrict
;;     This means the kind of the generated contract can not be greater than the supplied kind.
;;
;;   close-loop: (lisotf identifier?) (listof contract-restrict?) contract-restrict? -> contract-restrict?
;;     This takes a bunch of recursive contract identifiers, their corresponding contract-restricts,
;;     the contract restrict for a body and constructs the appropriate constract restrict.
;;
;; Other:
;;   validate-constraints: contract-restrict? -> void?
;;     This takes a contract-restrict and raises an exception if it has any violated constraints.
;;
;;   contract-restrict-recursive-values: contract-restrict? -> (dict/c identifier? kind?)
;;     Provides the kinds of all of the internal recursive contracts that are a part of the
;;     contract-restrict.
;;
;;
;;

(require
  "../utils/utils.rkt"
  racket/match
  racket/list
  racket/format
  racket/function
  racket/set
  (contract-req)
  syntax/private/id-table
  "kinds.rkt"
  "equations.rkt")

(provide
  simple-contract-restrict
  variable-contract-restrict
  merge-restricts*
  merge-restricts
  close-loop
  contract-restrict-recursive-values
  contract-restrict?
  contract-restrict-value
  kind-max-max)

(module structs racket/base
  (require "../utils/utils.rkt"
           (contract-req)
           racket/match
           racket/list
           racket/set
           syntax/private/id-table
           "kinds.rkt")
  
  (define free-id-set? free-id-table?)

  (struct constraint (value max) #:transparent)
  (struct kind-max (variables max) #:transparent
          #:methods gen:custom-write
          [(define (write-proc v port mode)
             (match-define (kind-max variables max) v)
             (define recur
               (case mode
                 [(#t) write]
                 [(#f) display]
                 [else (lambda (p port) (print p port mode))]))
             (define-values (open close)
               (if (equal? mode 0)
                   (values "(" ")")
                   (values "#<" ">")))
             (display open port)
             (fprintf port "kind-max")
             (display " " port)
             (display (map syntax-e (free-id-table-keys variables)) port)
             (display " " port)
             (recur max port)
             (display close port))])
  (struct contract-restrict (value recursive-values constraints)
          #:methods gen:custom-write
          [(define (write-proc v port mode)
             (match-define (contract-restrict value recursive-values constraints) v)
             (define recur
               (case mode
                 [(#t) write]
                 [(#f) display]
                 [else (lambda (p port) (print p port mode))]))
             (define-values (open close)
               (if (equal? mode 0)
                   (values "(" ")")
                   (values "#<" ">")))
             (display open port)
             (fprintf port "contract-restrict")
             (display " " port)
             (recur value port)

             (display " (" port)
             (define (recur-pair name val)
               (fprintf port "(~a " (syntax->datum name))
               (recur val port)
               (display ")" port))
             (define-values (names vals)
               (for/lists (_1 _2) ([(id val) (in-free-id-table recursive-values)])
                 (values id val)))
             (when (cons? names)
               (recur-pair (first names) (first vals))
               (for ([name (in-list (rest names))]
                     [val (in-list (rest vals))])
                    (display " " port)
                    (recur-pair name val)))
             (display ") " port)
             (recur constraints port)
             (display close port))]
          #:transparent)
  (provide/cond-contract
   ;; constraint: value must be below max
   [struct constraint ([value kind-max?] [max contract-kind?])]
   ;; kind-max: represents the maximum kind across all of the variables and the specified kind
   [struct kind-max ([variables free-id-set?] [max contract-kind?])]
   ;; contract-restrict: represents a contract with value, recursive-values maps mentioned
   ;; recursive parts to kind-maxes, constraints are constraints that need to hold
   [struct contract-restrict ([value kind-max?]
                              [recursive-values free-id-table?]
                              [constraints (set/c constraint?)])]))

(require 'structs)
(provide (struct-out kind-max))

(struct exn:fail:constraint-failure exn:fail (reason))

(define (free-id-set . elems)
  (for/fold ([table (make-immutable-free-id-table)])
            ([e (in-list elems)])
    (free-id-table-set table e #t)))

(define (free-id-set-union tables)
  (for*/fold ([table (make-immutable-free-id-table)])
             ([new-table (in-list tables)]
              [(k _) (in-free-id-table new-table)])
    (free-id-table-set table k #t)))

(define (free-id-table-union tables)
  (for*/fold ([table (make-immutable-free-id-table)])
             ([new-table (in-list tables)]
              [(k v) (in-free-id-table new-table)])
    (free-id-table-set table k v)))

(define (simple-contract-restrict kind)
  (contract-restrict (kind-max (free-id-set) kind) (make-immutable-free-id-table) (set)))
(define (variable-contract-restrict var)
  (contract-restrict (kind-max (free-id-set var) 'flat) (make-immutable-free-id-table) (set)))

(define (reason-string actual bound)
  (define (name k)
    (case k
      [(flat chaperone) (~a "a " k " contract")]
      [(impersonator) "an impersonator contract"]))
  (~a "required " (name bound) " but generated " (name actual)))


(define (trivial-constraint? con)
  (match con
    [(constraint _ 'impersonator)
     #t]
    [(constraint (kind-max (app free-id-table-count 0) actual) bound)
     (contract-kind<= actual bound)]
    [else #f]))


(define (add-constraint cr max)
  (match cr
    [(contract-restrict v rec constraints)
     (define con (constraint v max))
     (if (trivial-constraint? con)
         cr
         (contract-restrict v rec (set-add constraints con)))]))

(define (add-recursive-values cr dict) 
  (match cr
    [(contract-restrict v rec constraints)
     (contract-restrict v (free-id-table-union (list rec dict)) constraints)]))

(define (merge-restricts* min crs)
  (apply merge-restricts min crs))

(define (merge-restricts min . crs)
  (match crs
    [(list (contract-restrict vs rec constraints) ...)
     (contract-restrict (merge-kind-maxes min vs)
                        (free-id-table-union rec)
                        (apply set-union (set) constraints))]))

(define (merge-kind-maxes min-kind vs)
  (match vs
    [(list (kind-max variables maxes) ...)
     (kind-max (free-id-set-union variables) (apply combine-kinds min-kind maxes))]))

(define (close-loop names crs body)
  (define eqs (make-equation-set))
  (define vars
    (for/fold ([t (make-immutable-free-id-table)])
              ([name (in-list names)])
      (free-id-table-set t name 
                         (add-variable! eqs (simple-contract-restrict 'flat)))))
  (define (variable-lookup name)
    (variable-ref (free-id-table-ref vars name)))


  (define (instantiate-cr cr lookup-id)
    (define (instantiate-kind-max km)
      (match km
        [(kind-max ids actual)
         (define-values (bvals unbound-ids)
           (for/fold ([bvals '()] [ubids (make-immutable-free-id-table)])
                     ([(id _) (in-free-id-table ids)])
             (if (member id names)
                 (values (cons (contract-restrict-value (lookup-id id)) bvals) ubids)
                 (values bvals (free-id-table-set ubids id #t)))))
         (merge-kind-maxes 'flat (cons (kind-max unbound-ids actual) bvals))]))

    (define (instantiate-constraint con)
      (match con
        [(constraint km bound)
         (constraint (instantiate-kind-max km) bound)]))

    (match cr
      [(contract-restrict (kind-max ids max) rec constraints)
       (define-values (bound-vals unbound-ids)
         (for/fold ([bvs '()] [ubids (make-immutable-free-id-table)])
                   ([(id _) (in-free-id-table ids)])
           (if (member id names)
               (values (cons (lookup-id id) bvs) ubids)
               (values bvs (free-id-table-set ubids id #t)))))
       (merge-restricts* 'flat (cons
                                (contract-restrict
                                 (kind-max unbound-ids max) 
                                 rec
                                 (for*/set ([c (in-immutable-set constraints)]
                                            [ic (in-value (instantiate-constraint c))]
                                            #:when (not (trivial-constraint? ic)))
                                   ic))
                                bound-vals))]))

  (for ([name (in-list names)]
        [cr (in-list crs)])
    (add-equation! eqs
      (free-id-table-ref vars name)
      (λ () (instantiate-cr cr variable-lookup))))

  (define var-values (resolve-equations eqs))
  (define-values (id-values new-rec-values)
    (for*/fold ([id-vals (make-immutable-free-id-table)]
               [new-rec-vals (make-immutable-free-id-table)])
               ([(name var) (in-free-id-table vars)]
                [val (in-value (hash-ref var-values var))])
      (values (free-id-table-set id-vals name val)
              (free-id-table-set new-rec-vals name (contract-restrict-value val)))))

  (for*/fold ([cr (add-recursive-values
                   (instantiate-cr body (λ (id) (free-id-table-ref id-values id)))
                   new-rec-values)])
             ([(_ vals) (in-free-id-table id-values)]
              [vals (in-value (contract-restrict-recursive-values vals))])
    (add-recursive-values cr vals)))



(define (validate-constraints cr)
  (match cr
    [(contract-restrict (kind-max (app free-id-table-count 0) _) rec constraints)
     (for ([const (in-immutable-set constraints)])
       (match const
        [(constraint (kind-max (app free-id-table-count 0) kind) bound)
         (unless (contract-kind<= kind bound)
           (define reason (reason-string kind bound))
           (raise (exn:fail:constraint-failure
                    (format "Violated constraint: ~a" reason)
                    (current-continuation-marks)
                    reason)))]
         [(contract-restrict (kind-max d _) rec constraints)
          (raise (exn:fail:constraint-failure
                  (format "Violated constraint: ~a" "too many ids")
                  (current-continuation-marks)
                  (~a d)))]))]
    [(contract-restrict (kind-max d _) rec constraints)
     (raise (exn:fail:constraint-failure
                    (format "Violated constraint: ~a" "too many ids")
                    (current-continuation-marks)
                    (~a d)))]))


(provide/cond-contract
 [exn:fail:constraint-failure? predicate/c]
 [exn:fail:constraint-failure-reason (exn:fail:constraint-failure? . -> . string?)]
 [validate-constraints (contract-restrict? . -> . void?)]
 [add-constraint (contract-restrict? contract-kind? . -> . contract-restrict?)])

