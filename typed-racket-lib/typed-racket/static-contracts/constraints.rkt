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
;;   merge-restricts*: kind? (listof contracct-restrict?) -> contract-restrict?
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
;;   contract-restrict-recursive-values: contract-restrict? -> (free-id-table/c kind?)
;;     Provides the kinds of all of the internal recursive contracts that are a part of the
;;     contract-restrict.
;;
;;
;;

(require
  "../utils/utils.rkt"
  (utils hset fid-hset fid-hash)
  racket/match
  racket/format
  (contract-req)
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
           (utils hset fid-hset fid-hash)
           (contract-req)
           racket/match
           racket/list
           "kinds.rkt")

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
             (display (fid-hset-map variables syntax-e) port)
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
                (let ([keys (fid-hash-keys recursive-values)])
                  (values keys
                          (for/list ([key (in-list keys)])
                            (fid-hash-ref recursive-values)))))
             (when (cons? names)
               (recur-pair (first names) (first vals))
               (for ((name (rest names))
                     (val (rest vals)))
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
   [struct kind-max ([variables fid-hset?] [max contract-kind?])]
   ;; contract-restrict: represents a contract with value, recursive-values maps mentioned
   ;; recursive parts to kind-maxes, constraints are constraints that need to hold
   [struct contract-restrict ([value kind-max?]
                              [recursive-values (fid-hashof kind-max?)]
                              [constraints (hsetof constraint?)])]))

(require 'structs)
(provide (struct-out kind-max))

(struct exn:fail:constraint-failure exn:fail (reason))

(define/cond-contract (fid-hset-union* sets)
  (-> (listof fid-hset?) fid-hset?)
  (match sets
    [(cons set sets)
     (for/fold ([set set])
               ([new-set (in-list sets)])
       (fid-hset-union set new-set))]
    [_ (fid-hset)]))

(define/cond-contract (fid-hash-union t1 t2)
  (-> (fid-hashof any/c) (fid-hashof any/c) (fid-hashof any/c))
  (for*/fold ([acc t1])
             ([b (in-fid-hash-buckets t2)]
              [id/val (in-fid-hash-bucket b)])
    (fid-hash-set acc (car id/val) (cdr id/val))))

(define/cond-contract (fid-hash-union* hashes)
  (-> (listof (fid-hashof any/c)) (fid-hashof any/c))
  (match hashes
    [(cons hash hashes)
     (for*/fold ([acc hash])
                ([h (in-list hashes)]
                 [b (in-fid-hash-buckets h)]
                 [id/val (in-fid-hash-bucket b)])
       (fid-hash-set acc (car id/val) (cdr id/val)))]
    [_ (fid-hash)]))


(define (simple-contract-restrict kind)
  (contract-restrict (kind-max (fid-hset) kind)
                     (fid-hash)
                     (hset)))
(define (variable-contract-restrict var)
  (contract-restrict (kind-max (fid-hset var) 'flat)
                     (fid-hash)
                     (hset)))

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
    [(constraint (kind-max (? fid-hset-empty?) actual) bound)
     (contract-kind<= actual bound)]
    [else #f]))


(define (add-constraint cr max)
  (match cr
    [(contract-restrict v rec constraints)
     (define con (constraint v max))
     (if (trivial-constraint? con)
         cr
         (contract-restrict v rec (hset-add constraints con)))]))

(define (add-recursive-values cr dict) 
  (match cr
    [(contract-restrict v rec constraints)
     (contract-restrict v (fid-hash-union rec dict) constraints)]))

(define/cond-contract (merge-restricts* min crs)
  (-> contract-kind? (listof contract-restrict?) contract-restrict?)
  (apply merge-restricts min crs))

(define (merge-restricts min . crs)
  (match crs
    [(list (contract-restrict vs rec constraints) ...)
     (contract-restrict (merge-kind-maxes min vs)
                        (fid-hash-union* rec)
                        (for/fold ([h (hset)])
                                  ([c (in-list constraints)])
                          (hset-union h c)))]))

(define (merge-kind-maxes min-kind vs)
  (match vs
    [(list (kind-max variables maxes) ...)
     (kind-max (fid-hset-union* variables)
               (apply combine-kinds min-kind maxes))]))

(define/cond-contract (close-loop names crs body)
  (-> (listof identifier?) (listof contract-restrict?) contract-restrict?
      contract-restrict?)
  
  (define eqs (make-equation-set))
  (define vars
    (for/fid-hash ([name (in-list names)])
      (values name (add-variable! eqs (simple-contract-restrict 'flat)))))
  (define (variable-lookup name)
    (variable-ref (fid-hash-ref vars name)))


  (define (instantiate-cr cr lookup-id)
    (define (instantiate-kind-max km)
      (match km
        [(kind-max ids actual)
         (define-values (bound-ids-list unbound-ids-set)
           (for*/fold ([bids '()] [ubids (fid-hset)])
                      ([b (in-fid-hset-buckets ids)]
                       [id (in-fid-hset-bucket b)])
             (if (member id names)
                 (values (cons (contract-restrict-value (lookup-id id)) bids)
                         ubids)
                 (values bids
                         (fid-hset-add ubids id)))))
         (merge-kind-maxes 'flat (cons (kind-max unbound-ids-set actual)
                                       bound-ids-list))]))

    (define (instantiate-constraint con)
      (match con
        [(constraint km bound)
         (constraint (instantiate-kind-max km) bound)]))

    (match cr
      [(contract-restrict (kind-max ids max) rec constraints)
       (define-values (bound-ids unbound-ids)
         (for*/fold ([bids '()] [ubids (fid-hset)])
                    ([b (in-fid-hset-buckets ids)]
                     [id (in-fid-hset-bucket b)])
           (if (member id names)
               (values (cons id bids) ubids)
               (values bids (fid-hset-add ubids id)))))
       (merge-restricts* 'flat (cons
                                 (contract-restrict
                                   (kind-max unbound-ids max) 
                                   rec
                                   (for*/hset ([elem (in-hset constraints)]
                                               [elem (in-value (instantiate-constraint elem))]
                                               #:when (not (trivial-constraint? elem)))
                                     elem))
                                 (map lookup-id bound-ids)))]))

  (for ([name (in-list names)]
        [cr (in-list crs)])
    (add-equation! eqs
                   (fid-hash-ref vars name)
                   (λ () (instantiate-cr cr variable-lookup))))

  (define var-values (resolve-equations eqs))
  (define id-values
    (for*/fold ([t (fid-hash)])
               ([b (in-fid-hash-buckets vars)]
                [id/var (in-fid-hash-bucket b)])
      (fid-hash-set t (car id/var) (hash-ref var-values (cdr id/var)))))

  (define new-rec-values
    (for*/fold ([t (fid-hash)])
               ([b (in-fid-hash-buckets id-values)]
                [id/val (in-fid-hash-bucket b)])
      (fid-hash-set t (car id/val) (contract-restrict-value (cdr id/val)))))

  (for/fold ([cr (instantiate-cr body (lambda (id) (fid-hash-ref id-values id)))])
            ([rec-values (cons new-rec-values (map contract-restrict-recursive-values
                                                   (fid-hash-values id-values)))])
    (add-recursive-values cr rec-values)))



(define (validate-constraints cr)
  (match cr
    [(contract-restrict (kind-max (? fid-hset-empty?) _) rec constraints)
     (for ([const (in-hset constraints)])
       (match const
        [(constraint (kind-max (? fid-hset-empty?) kind) bound)
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

