#lang racket/base

(require racket/contract)

(provide (rename-out [-prefab/c prefab/c]))

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; prefab contract property functions
;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

;; name
(define (prefab/c-name ctc)
  (define key (base-prefab/c-key ctc))
  (define field-contracts (base-prefab/c-field-contracts ctc))
  `(prefab/c ,key ,@(map contract-name field-contracts)))

;; flat-first-order
(define (prefab/c-flat-first-order ctc)
  (define field-contracts (base-prefab/c-field-contracts ctc))
  (define prefab-struct-type (prefab-key->struct-type (base-prefab/c-key ctc)
                                                      (length field-contracts)))
  (define pred? (struct-type-make-predicate prefab-struct-type))
  (define accessor-proc
    (let-values ([(name
                   init-field-cnt
                   auto-field-cnt
                   accessor-proc
                   mutator-proc
                   immutable-k-list
                   super-type 
                   skipped?)
                  (struct-type-info prefab-struct-type)])
      accessor-proc))
  (cond
    [(flat-prefab/c? ctc)
     ;; It's flat, so each field and contract must be immutable and
     ;; flat respectively.
     (λ (val)
       (and (pred? val)
            (for/and ([field-contract (in-list field-contracts)]
                      [idx (in-naturals)])
              ((flat-contract-predicate field-contract) (accessor-proc val idx)))))]
    [else
     ;; flat-field-indices+contracts : (listof (cons natural contract))
     ;; i.e. which fields have flat contracts?
     (define flat-field-indices+contracts
       (for/list ([field-contract (in-list field-contracts)]
                  [idx (in-naturals)]
                  #:when (flat-contract? field-contract))
         (cons idx field-contract)))
     (λ (val)
       (and (pred? val)
            (for*/and ([idx/contract (in-list flat-field-indices+contracts)]
                       [idx (in-value (car idx/contract))]
                       [field-contract (in-value (cdr idx/contract))]
                       [field-val (in-value (accessor-proc val idx))])
              ((flat-contract-predicate field-contract) field-val))))]))

(define (ending n)
  (case (remainder n 100)
    [(11 12 13) "th"]
    [else (case (remainder n 10)
            [(1) "st"]
            [(2) "nd"]
            [(3) "rd"]
            [else "th"])]))

;; late-neg-projection
(define (prefab/c-late-neg-projection ctc)
  (define field-contracts (base-prefab/c-field-contracts ctc))
  (define field-count (length field-contracts))
  (define mutability-bits (base-prefab/c-mutability-bits ctc))
  (define prefab-struct-type (prefab-key->struct-type (base-prefab/c-key ctc)
                                                      field-count))
  (define pred? (struct-type-make-predicate prefab-struct-type))
  (define-values (accessor-proc mutator-proc)
    (let-values ([(_1 _2 _3 accessor-proc mutator-proc _6 _7 _8)
                  (struct-type-info prefab-struct-type)])
      (values accessor-proc mutator-proc)))
  (define chaperone? (andmap chaperone-contract? field-contracts))
  (define (generate-field-functions/contracts first-projs
                                              late-acc-projs
                                              late-mut-projs
                                              val
                                              b-)
    (for/fold ([ctcs '()])
              ([first-proj (in-list first-projs)]
               [late-acc-proj (in-list late-acc-projs)]
               [late-mut-proj (in-list late-mut-projs)]
               [idx (in-naturals)])
      ;; first order check on value
      ((first-proj (accessor-proc val idx)) b-)
      (let ([ctcs/accessor (if (flat-contract? ctc)
                               ctcs
                               (list* (make-struct-field-accessor accessor-proc idx)
                                      (λ (self val) (late-acc-proj val b-))
                                      ctcs))])
        (if (bitwise-bit-set? mutability-bits idx)
            (list* (make-struct-field-mutator mutator-proc idx)
                   (λ (self val) (late-mut-proj val b-))
                   ctcs/accessor)
            ctcs/accessor))))
  (λ (b+)
    (define-values (first-projs late-acc-projs late-mut-projs)
      (for/lists (_1 _2 _3)
                 ([field-ctc (in-list field-contracts)]
                  [idx (in-naturals)])
        (define field-context
          (format "the ~a~a field of"
                  (add1 idx) (ending (add1 idx))))
        (values ((get/build-val-first-projection field-ctc)
                 (blame-add-context b+ field-context))
                ((get/build-late-neg-projection field-ctc)
                 (blame-add-context b+ field-context))
                ((get/build-late-neg-projection field-ctc)
                 (blame-add-context b+ field-context #:swap? #t)))))
    (λ (val b-)
      (unless (pred? val)
        (raise-blame-error b+ #:missing-party b-
                           val '(expected: "~a" given: "~e")
                           (contract-name ctc)
                           val))
      (apply
       (if chaperone? chaperone-struct impersonate-struct)
       val
       prefab-struct-type
       (generate-field-functions/contracts first-projs
                                           late-acc-projs
                                           late-mut-projs
                                           val
                                           b-)))))

(define (prefab-sub-key? this-key this-field-count that-key that-field-count)
  ;; TODO this could easily be more complete
  (and (equal? this-key that-key)
       (= this-field-count that-field-count)))

;; stronger
(define (prefab/c-stronger this that)
  (cond
    [(not (base-prefab/c? that)) #f]
    [else
     (define this-key (base-prefab/c-key this))
     (define these-field-contracts (base-prefab/c-field-contracts this))
     (define this-mutability-bits (base-prefab/c-mutability-bits this))
     (define this-field-count (length these-field-contracts))
     (define that-key (base-prefab/c-key that))
     (define those-field-contracts (base-prefab/c-field-contracts that))
     (define that-field-count (length those-field-contracts))
     (cond
       [(not (prefab-sub-key? this-key this-field-count that-key that-field-count))
        #f]
       [else
        (for/and ([this-field-contract (in-list these-field-contracts)]
                  [that-field-contract (in-list those-field-contracts)]
                  [idx (in-naturals)])
          (cond
            [(bitwise-bit-set? this-mutability-bits idx)
             (contract-equivalent? this-field-contract that-field-contract)]
            [else
             (contract-stronger? this-field-contract that-field-contract)]))])]))

;; equivalent
(define (prefab/c-equivalent this that)
  (cond
    [(not (and (base-prefab/c? that)
               (equal? (base-prefab/c-key this) (base-prefab/c-key that))))
     #f]
    [else
     (define these-field-contracts (base-prefab/c-field-contracts this))
     (define those-field-contracts (base-prefab/c-field-contracts that))
     (and (eqv? (length these-field-contracts)
                (length those-field-contracts))
          (andmap contract-equivalent? these-field-contracts those-field-contracts))]))

;; generate
(define ((prefab/c-generate ctc) fuel)
  (define field-contracts (base-prefab/c-field-contracts ctc))
  (define total-field-count (length field-contracts))
  (define prefab-struct-type (prefab-key->struct-type (base-prefab/c-key ctc)
                                                      total-field-count))
  (define constructor (struct-type-make-constructor prefab-struct-type))
  (let loop ([to-gen field-contracts]
             [gens '()])
    (cond
      [(null? to-gen)
       (λ ()
         (let loop ([gens gens]
                    [args '()])
           (cond
             [(null? gens) (apply constructor args)]
             [else (loop (cdr gens)
                         (cons ((car gens)) args))])))]
      [else
       (define field-contract (car to-gen))
       (define field-gen (contract-random-generate/choose field-contract fuel))
       (cond
         [field-gen (loop (cdr to-gen) (cons field-gen gens))]
         [else #f])])))


;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; prefab contract structs
;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; NOTE: these are meant to more-or-less mirror struct/c
;; in how they work. One notable difference: we don't
;; necessarily have identifiers corresponding to accessor
;; functions for prefabs.



(struct base-prefab/c (key field-contracts mutability-bits))

(struct flat-prefab/c base-prefab/c ()
  #:property prop:flat-contract
  (build-flat-contract-property
   #:name prefab/c-name
   #:first-order prefab/c-flat-first-order
   #:late-neg-projection #f
   #:stronger prefab/c-stronger
   #:equivalent prefab/c-equivalent
   #:generate prefab/c-generate))

(struct prefab/c base-prefab/c ()
  #:property prop:chaperone-contract
  (build-chaperone-contract-property
   #:name prefab/c-name
   #:first-order prefab/c-flat-first-order
   #:late-neg-projection prefab/c-late-neg-projection
   #:stronger prefab/c-stronger
   #:equivalent prefab/c-equivalent
   #:generate prefab/c-generate
   #:exercise #f))

(struct impersonator-prefab/c base-prefab/c ()
  #:property prop:contract
  (build-contract-property
   #:name prefab/c-name
   #:first-order prefab/c-flat-first-order
   #:late-neg-projection prefab/c-late-neg-projection
   #:stronger prefab/c-stronger
   #:equivalent prefab/c-equivalent
   #:generate prefab/c-generate
   #:exercise #f))


;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; prefab contract constructor
;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;;
;; creates a contract for a prefab with key `key`
;; and with `(length args)` fields
(define (-prefab/c key . args)
  (unless (prefab-key? key)
    (raise-argument-error 'prefab/c "prefab-key?" key))
  (define field-contracts (for/list ([arg (in-list args)])
                            (coerce-contract 'prefab/c arg)))
  ;; construct struct type and other struct information so we can
  ;; reject nonsensical prefabs early and determine if this will
  ;; be a flat contract or not
  (define field-count (length field-contracts))
  (define prefab-struct-type (prefab-key->struct-type key field-count))
  (define immutable-k-list
    (let-values ([(_1 _2 _3 _4 _5 immutable-k-list _7 _8)
                  (struct-type-info prefab-struct-type)])
      immutable-k-list))
  (define mutability-bits (for/fold ([bits #b0])
                                    ([idx (in-range field-count)]
                                     #:when (not (memv idx immutable-k-list)))
                            (bitwise-ior bits (arithmetic-shift 1 idx))))
  (define max-kind (for/fold ([kind 0])
                             ([ctc (in-list field-contracts)])
                     (max kind (cond
                                 [(flat-contract? ctc) 0]
                                 [(chaperone-contract? ctc) 1]
                                 [else 2]))))
  (case max-kind
    [(0) (if (= (length immutable-k-list) field-count)
             (flat-prefab/c key field-contracts mutability-bits)
             (prefab/c key field-contracts mutability-bits))]
    [(1) (prefab/c key field-contracts mutability-bits)]
    [else (impersonator-prefab/c key field-contracts mutability-bits)]))
