#lang racket/base

(require racket/contract)

(provide (rename-out [-prefab/c prefab/c]))

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; prefab contract property functions
;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

;; name
(define (prefab/c-name ctc)
  `(prefab/c ',(prefab-struct-type-key (base-prefab/c-struct-type ctc))
             ,@(for/list ([spec (in-list (base-prefab/c-field-specs ctc))])
                 (contract-name (field-spec-contract spec)))))

(define (prefab-struct-type-key prefab-struct-type)
  (define ctor (struct-type-make-constructor prefab-struct-type))
  (prefab-struct-key
   (apply ctor (build-list (procedure-arity ctor) ;; will be int
                           void))))

;; flat-first-order
(define-values [prefab/c-flat-first-order prefab/c-first-order]
  (let ()
    (define ((make-first-order-proc get-predicate) ctc)
      (define specs (base-prefab/c-field-specs ctc))
      (define prefab-struct-type (base-prefab/c-struct-type ctc))
      (define pred? (struct-type-make-predicate prefab-struct-type))
      (λ (val)
        (and (pred? val)
             (for/and ([spec (in-list specs)])
               (define ok?
                 (get-predicate (field-spec-contract spec)))
               (define field-val
                 ((field-spec-accessor spec) val))
               (ok? field-val)))))
    (values (make-first-order-proc flat-contract-predicate)
            (make-first-order-proc contract-first-order))))

(define (ending n)
  (case (remainder n 100)
    [(11 12 13) "th"]
    [else (case (remainder n 10)
            [(1) "st"]
            [(2) "nd"]
            [(3) "rd"]
            [else "th"])]))

;; late-neg-projection
(define (prefab/c-late-neg-projection whole-ctc)
  (define field-specs (base-prefab/c-field-specs whole-ctc))
  (define prefab-struct-type (base-prefab/c-struct-type whole-ctc))
  (define pred? (struct-type-make-predicate prefab-struct-type))
  (λ (b+)
    ;; Prepare projections:
    (define-values [field-blames acc-projs mut-projs]
      (for/lists (_1 _2 _3)
                 ([spec (in-list field-specs)]
                  [pos (in-naturals 1)])
        (define field-late-neg
          (get/build-late-neg-projection (field-spec-contract spec)))
        (define field-pos-blame
          (blame-add-context b+ (format "the ~a~a field of" pos (ending pos))))
        (values field-pos-blame
                (field-late-neg field-pos-blame)
                (and (field-spec-maybe-mutator spec)
                     (field-late-neg (blame-swap field-pos-blame))))))
    (λ (val neg-party)
      ;; Check correct struct type:
      (unless (pred? val)
        (raise-blame-error b+ #:missing-party neg-party
                           val '(expected: "~a" given: "~e")
                           (object-name pred?)
                           val))
      ;; Check and potentially wrap fields:
      ;; `impersonate-struct` can't wrap immutable fields,
      ;; so we must gather args separately
      (define (wrap imp/chap-struct val args)
        (if (null? args)
            val
            (apply imp/chap-struct val prefab-struct-type args)))
      (for/fold ([chap-args null]
                 [imp-args null]
                 #:result (wrap chaperone-struct
                                (wrap impersonate-struct val imp-args)
                                chap-args))
                ([field-blame (in-list field-blames)]
                 [acc-proj+blame (in-list acc-projs)]
                 [maybe-mut-proj+blame (in-list mut-projs)]
                 [spec (in-list field-specs)])
        (define blame+neg-party (cons field-blame neg-party))
        (define field-accessor (field-spec-accessor spec))
        (define (chk self val)
          (with-contract-continuation-mark
           blame+neg-party
           (acc-proj+blame val neg-party)))
        ;; First-order check on value:
        (chk #f (field-accessor val)) ;; struct/c omits this for mutable fields
        ;; Add wrappers, if any:
        (cond
          [maybe-mut-proj+blame
           (define (add-args args)
             (list* field-accessor
                    chk
                    (field-spec-maybe-mutator spec)
                    (λ (self val)
                      (with-contract-continuation-mark
                       ;; struct/c uses same blame+neg-party for accessor & mutator
                       blame+neg-party
                       (maybe-mut-proj+blame val neg-party)))
                    args))
           (if (impersonator-contract? (field-spec-contract spec))
               (values chap-args (add-args imp-args))
               (values (add-args chap-args) imp-args))]
          [else
           (values (if (flat-contract? (field-spec-contract spec))
                       chap-args
                       (list* field-accessor chk chap-args))
                   imp-args)])))))

(define (prefab-subtype? this-prefab-type that-prefab-type)
  (let loop ([this-prefab-type this-prefab-type])
    (or (equal? this-prefab-type that-prefab-type)
        (let-values ([(name
                       init-field-count
                       auto-field-count
                       accessor-proc
                       mutator-proc
                       immutable-k-list
                       super-type
                       skipped?)
                      (struct-type-info this-prefab-type)])
          (and super-type
               (loop super-type))))))

;; stronger
(define (prefab/c-stronger this that)
  (cond
    [(not (base-prefab/c? that)) #f]
    [(not (prefab-subtype? (base-prefab/c-struct-type this)
                           (base-prefab/c-struct-type that)))
     #f]
    [else
     (for/and ([this-field-spec (in-list (base-prefab/c-field-specs this))]
               [that-field-spec (in-list (base-prefab/c-field-specs that))])
       (define this-field-contract (field-spec-contract this-field-spec))
       (define that-field-contract (field-spec-contract that-field-spec))
       (cond
         [(field-spec-maybe-mutator this-field-spec)
          (contract-equivalent? this-field-contract that-field-contract)]
         [else
          (contract-stronger? this-field-contract that-field-contract)]))]))


;; equivalent
(define (prefab/c-equivalent this that)
  (cond
    [(not (and (base-prefab/c? that)
               (equal? (base-prefab/c-struct-type this)
                       (base-prefab/c-struct-type that))))
     #f]
    [else
     (define these-field-specs (base-prefab/c-field-specs this))
     (define those-field-specs (base-prefab/c-field-specs that))
     (and (eqv? (length these-field-specs)
                (length those-field-specs))
          (for/and ([this-spec (in-list these-field-specs)]
                    [that-spec (in-list those-field-specs)])
            (contract-equivalent? (field-spec-contract this-spec)
                                  (field-spec-contract that-spec))))]))

;; generate
(define ((prefab/c-generate ctc) fuel)
  (define field-specs (base-prefab/c-field-specs ctc))
  (define prefab-struct-type (base-prefab/c-struct-type ctc))
  (define constructor (struct-type-make-constructor prefab-struct-type))
  (let loop ([to-gen field-specs]
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
      [(field-spec-auto? (car to-gen))
       (loop (cdr to-gen) gens)]
      [else
       (define field-contract (field-spec-contract (car to-gen)))
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

(struct base-prefab/c (struct-type field-specs))

(struct field-spec (contract accessor maybe-mutator auto?))

(struct flat-prefab/c base-prefab/c ()
  #:property prop:custom-write contract-custom-write-property-proc
  #:property prop:flat-contract
  (build-flat-contract-property
   #:name prefab/c-name
   #:first-order prefab/c-flat-first-order
   #:late-neg-projection #f
   #:stronger prefab/c-stronger
   #:equivalent prefab/c-equivalent
   #:generate prefab/c-generate))

(struct prefab/c base-prefab/c ()
  #:property prop:custom-write contract-custom-write-property-proc
  #:property prop:chaperone-contract
  (build-chaperone-contract-property
   #:name prefab/c-name
   #:first-order prefab/c-first-order
   #:late-neg-projection prefab/c-late-neg-projection
   #:stronger prefab/c-stronger
   #:equivalent prefab/c-equivalent
   #:generate prefab/c-generate
   #:exercise #f))

(struct impersonator-prefab/c base-prefab/c ()
  #:property prop:custom-write contract-custom-write-property-proc
  #:property prop:contract
  (build-contract-property
   #:name prefab/c-name
   #:first-order prefab/c-first-order
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
  (define (raise-arg-error expected pos)
    (apply raise-argument-error 'prefab/c expected pos key args))
  (unless (prefab-key? key)
    (raise-arg-error "prefab-key?" 0))
  (define field-contracts (for/list ([arg (in-list args)])
                            (coerce-contract 'prefab/c arg)))
  ;; construct struct type and other struct information so we can
  ;; reject nonsensical prefabs early and determine if this will
  ;; be a flat contract or not
  (define field-count (length field-contracts))
  (define prefab-struct-type (prefab-key->struct-type key field-count))
  (define field-specs
    (let-values ([(accessors mutators auto?s)
                  (build-field-accessors+mutators prefab-struct-type)])
      (for ([pos (in-naturals 1)]
            [ctc (in-list field-contracts)]
            [maybe-mutator (in-list mutators)]
            #:unless (or maybe-mutator
                         (chaperone-contract? ctc)))
        (raise-arg-error (format "a chaperone-contract? for the immutable ~a~a field"
                                 pos (ending pos))
                         pos))
      (map field-spec field-contracts accessors mutators auto?s)))
  (define max-kind (for/fold ([kind 0])
                             ([ctc (in-list field-contracts)])
                     (max kind (cond
                                 [(flat-contract? ctc) 0]
                                 [(chaperone-contract? ctc) 1]
                                 [else 2]))))
  (case max-kind
    [(0) (if (ormap field-spec-maybe-mutator field-specs)
             (prefab/c prefab-struct-type field-specs)
             (flat-prefab/c prefab-struct-type field-specs))]
    [(1) (prefab/c prefab-struct-type field-specs)]
    [else (impersonator-prefab/c prefab-struct-type field-specs)]))


(define (build-field-accessors+mutators prefab-struct-type)
  (let loop ([prefab-struct-type prefab-struct-type]
             [accessors null]
             [mutators null]
             [auto?s null])
    (define-values [name
                    init-field-count
                    auto-field-count
                    accessor-proc
                    mutator-proc
                    immutable-k-list
                    super-type
                    skipped?]
      (struct-type-info prefab-struct-type))
    (define immediate-field-count
      (+ init-field-count auto-field-count))
    (for/fold ([accessors accessors]
               [mutators mutators]
               [auto?s auto?s]
               #:result (if super-type
                            (loop super-type accessors mutators auto?s)
                            (values accessors mutators auto?s)))
              ([idx (in-range (sub1 immediate-field-count) -1 -1)])
      (values (cons (make-struct-field-accessor accessor-proc idx)
                    accessors)
              (cons (and (not (memq idx immutable-k-list)) ;; idx is a fixnum
                         (make-struct-field-mutator mutator-proc idx))
                    mutators)
              (cons (>= idx init-field-count)
                    auto?s)))))
