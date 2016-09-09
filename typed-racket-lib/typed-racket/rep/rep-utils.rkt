#lang racket/base
(require "../utils/utils.rkt"
         "../utils/print-struct.rkt"
         
         racket/match
         racket/generic
         (contract-req)
         "free-variance.rkt"
         "type-mask.rkt"
         racket/stxparam
         syntax/parse/define
         syntax/id-table
         racket/unsafe/ops
         (for-syntax
          racket/match
          racket/list
          racket/sequence
          (except-in syntax/parse id identifier keyword)
          racket/base
          syntax/struct
          syntax/id-table
          (contract-req)
          racket/syntax))


(provide (all-defined-out)
         (for-syntax var-name))

(provide-for-cond-contract length>=/c)

(define-for-cond-contract ((length>=/c len) l)
  (and (list? l)
       (>= (length l) len)))

(define (Rep-seq x) (unsafe-struct-ref x 0))

;; seq: interning-generated serial number used to compare Reps (type<).
;; free-vars: cached free type variables
;; free-idxs: cached free dot sequence variables
;; stx: originating syntax for error-reporting
(struct Rep (sequence-number free-vars free-idxs) #:transparent
  #:methods gen:equal+hash
  [(define (equal-proc x y recur)
     (unsafe-fx= (Rep-seq x) (Rep-seq y)))
   (define (hash-proc x recur) (Rep-seq x))
   (define (hash2-proc x recur) (Rep-seq x))])

(define (Rep<? x y)
  (unsafe-fx< (Rep-seq x) (Rep-seq y)))

;; prop:get-values
(define-values (prop:Rep-name Rep-name)
  (let-values ([(prop _ accessor) (make-struct-type-property 'named)])
    (values prop accessor)))


;; prop:get-values
(define-values (prop:get-values-fun get-values-fun)
  (let-values ([(prop _ accessor) (make-struct-type-property 'values)])
    (values prop accessor)))
;; Rep-values
(define (Rep-values x)
  ((get-values-fun x) x))

;; prop:get-constructor
(define-values (prop:get-constructor-fun Rep-constructor)
  (let-values ([(prop _ accessor) (make-struct-type-property 'constructor)])
    (values prop accessor)))

;; structural type info for simple/straightforward types
;; (i.e. we store the list of field variances)
(define-values (prop:structural structural? Type-variances)
  (make-struct-type-property 'structural))

;; top type predicates
(define-values (prop:top-type has-top-type? top-type-pred)
  (make-struct-type-property 'top-type))

;; prop:walk-fun
(define-values (prop:walk-fun walkable? walk-fun)
  (make-struct-type-property 'walk))
;; Rep-walk
(define (Rep-walk f x)
  ((walk-fun x) f x))

;; prop:fold-fun
(define-values (prop:fold-fun foldable? fold-fun)
  (make-struct-type-property 'fold))

;; Rep-fold
(define (Rep-fold f x)
  ((fold-fun x) f x))


;; Is this a type that can be a 'back-edge' into the type graph?
;; (i.e. could blindly following this type lead to infinite recursion?)
(define-values (prop:resolvable needs-resolved?)
  (let-values ([(prop predicate _) (make-struct-type-property 'resolvable)])
    (values prop predicate)))


;;************************************************************
;; Rep Declaration Syntax Classes
;;************************************************************
(define (make-counter!)
  (let ([state 0])
    (λ () (begin0 state (set! state (unsafe-fx+ 1 state))))))

(define count! (make-counter!))
(define id-count! (make-counter!))

(define identifier-table (make-free-id-table))

(define (hash-id id)
  (free-id-table-ref!
   identifier-table
   id
   (λ () (let ([c (id-count!)])
           (free-id-table-set! identifier-table id c)
           c))))

(define (hash-name name)
  (if (identifier? name)
      (hash-id name)
      name))

(begin-for-syntax
  ;; #:frees definition parsing
  (define-syntax-class freesspec
    #:attributes (free-vars free-idxs)
    (pattern ([#:vars (f1) vars-body:expr]
              [#:idxs (f2) idxs-body:expr])
             #:with free-vars #'(let ([f1 Rep-free-vars]) vars-body)
             #:with free-idxs #'(let ([f2 Rep-free-idxs]) idxs-body))
    (pattern ((f:id) body:expr)
             #:with free-vars #'(let ([f Rep-free-vars]) body)
             #:with free-idxs #'(let ([f Rep-free-idxs]) body)))
  ;; #:fold definition parsing
  (define-syntax-class (walkspec name match-expdr struct-fields)
    #:attributes (def)
    (pattern ((f:id) body:expr)
             #:with def
             (with-syntax ([name name]
                           [(flds ...) struct-fields]
                           [mexpdr match-expdr])
               #'(λ (f self)
                   (match self
                     [(mexpdr flds ...) body]
                     [_ (error 'Rep-walk "bad match in ~a's walk" (quote name))])))))
  ;; #:map definition parsing
  (define-syntax-class (foldspec name match-expdr struct-fields)
    #:attributes (def)
    (pattern ((f:id (~optional (~seq #:self self:id)
                               #:defaults ([self (generate-temporary 'self)])))
              body:expr)
             #:with def
             (with-syntax ([name name]
                           [(flds ...) struct-fields]
                           [mexpdr match-expdr])
               #'(λ (f self)
                   (match self
                     [(mexpdr flds ...) body]
                     [_ (error 'Rep-fold "bad match in ~a's fold" (quote name))])))))
  ;; variant name parsing
  (define-syntax-class var-name
    #:attributes (name raw-constructor constructor mexpdr pred)
    (pattern name:id
             #:with raw-constructor
             ;; raw constructor should only be used by macros (hence the gensym)
             (format-id #'name "raw-make-~a" (gensym (syntax-e #'name)))
             #:with constructor
             (format-id #'name "make-~a" (syntax-e #'name))
             #:with mexpdr
             (format-id #'name "~a:" (syntax-e #'name))
             #:with pred
             (format-id #'name "~a?" (syntax-e #'name))))
  ;; structure accessor parsing
  (define-syntax-class (fld-id struct-name)
    #:attributes (name accessors)
    (pattern name:id
             #:with accessors
             (format-id #'name "~a-~a" (syntax-e struct-name) (syntax-e #'name))))
  ;; struct field name parsing
  (define-syntax-class (var-fields name)
    #:attributes ((ids 1)
                  (contracts 1)
                  (accessors 1))
    (pattern ([(~var ids (fld-id name))
               contracts:expr] ...)
             #:with (accessors ...) #'(ids.accessors ...))))

;;************************************************************
;; def-rep
;;************************************************************
;;
;; Declaration macro for Rep structures
(define-syntax (def-rep stx)
  (syntax-parse stx
    [(_
      ;; variant name
      var:var-name
      ;; fields and field contracts
      (~var flds (var-fields #'var.name))
      ;; options
      (~or
       ;; parent struct (if any)
       (~optional (~optional [#:parent parent:id])
                  #:defaults ([parent #'Rep]))
       ;; base declaration (i.e. no fold/map)
       (~optional (~and #:base base?))
       ;; All Reps are interned
       (~optional [#:intern-key provided-intern-key])
       ;; #:frees spec (how to compute this Rep's free type variables)
       (~optional [#:frees . frees-spec:freesspec])
       ;; #:walk spec (how to traverse this structure for effect)
       (~optional [#:walk . (~var walk-spec (walkspec #'var.name
                                                      #'var.mexpdr
                                                      #'(flds.ids ...)))])
       ;; #:fold spec (how to transform & fold this structure)
       (~optional [#:fold . (~var fold-spec (foldspec #'var.name
                                                      #'var.mexpdr
                                                      #'(flds.ids ...)))])
       (~optional [#:type-mask . type-mask-body])
       ;; is this a Type w/ a Top type? (e.g. Vector --> VectorTop)
       (~optional [#:top top-pred:id])
       ;; #:no-provide option (i.e. don't provide anything automatically)
       (~optional (~and #:needs-resolving needs-resolving?))
       ;; #:no-provide option (i.e. don't provide anything automatically)
       (~optional (~and #:no-provide no-provide?))
       ;; field variances (e.g. covariant/contravariant/etc) declarations
       (~optional (~and [#:variances variances ...] structural))
       ;; #:extras to specify other struct properties in a per-definition manner
       (~optional [#:extras . extras]))
      ...)

     ;; - - - - - - - - - - - - - - -
     ;; Error checking
     ;; - - - - - - - - - - - - - - -
     
     ;; build convenient boolean flags
     (define is-a-type? (eq? 'Type (syntax-e #'parent)))
     (define intern-key (if (attribute provided-intern-key)
                            #'provided-intern-key
                            #'#t))
     ;; intern-key is required (when the number of fields is > 0)
     (when (and (not (attribute provided-intern-key))
                (> (length (syntax->list #'flds)) 0))
       (raise-syntax-error 'def-rep "intern key specification required when the number of fields > 0"
                           #'var))
     ;; no frees, walk, or fold for #:base Reps
     (when (and (attribute base?) (or (attribute frees-spec)
                                      (attribute walk-spec)
                                      (attribute fold-spec)))
       (raise-syntax-error 'def-rep "base reps cannot have #:frees, #:walk, or #:fold"
                           #'var))
     ;; if non-base, frees, walk, and fold are required
     (when (and (not (attribute base?))
                (or (not (attribute frees-spec))
                    (not (attribute walk-spec))
                    (not (attribute fold-spec))))
       (raise-syntax-error 'def-rep "non-base reps require #:frees, #:walk, and #:fold"
                           #'var))
     ;; can't be structural and not a type
     (when (and (not is-a-type?) (attribute structural))
       (raise-syntax-error 'def-rep "only types can be structural" #'structural))

     ;; - - - - - - - - - - - - - - -
     ;; Let's build the definitions!
     ;; - - - - - - - - - - - - - - -
     
     (with-syntax*
       ([intern-key intern-key]
        ;; contract for constructor
        [constructor-contract #'(-> flds.contracts ... var.pred)]
        ;; match expander (skips 'meta' fields)
        [mexpdr-def
         #`(define-match-expander var.mexpdr
             (λ (s)
               (syntax-parse s
                 [(_ . pats)
                  #,(if is-a-type? ;; skip Type-mask and subtype cache
                        #'(syntax/loc s (var.name _ _ _ _ _ . pats))
                        #'(syntax/loc s (var.name _ _ _ . pats)))])))]
        ;; free var/idx defs
        [free-vars-def (cond
                         [(attribute base?) #'empty-free-vars]
                         [else #'frees-spec.free-vars])]
        [free-idxs-def (cond
                         [(attribute base?) #'empty-free-vars]
                         [else #'frees-spec.free-idxs])]
        ;; top type info
        [(maybe-top-type-spec ...)
         (if (attribute top-pred)
             #'(#:property prop:top-type top-pred)
             #'())]
        ;; if it's a structural type, save its field variances
        [(maybe-structural ...)
         (if (attribute structural)
             #'(#:property prop:structural (list variances ...))
             #'())]
        ;; an argument if we accept a type mask
        [mask-arg (generate-temporary 'mask)]
        ;; constructor w/ interning and Type-mask handeling if necessary
        [constructor-def
         (cond
           ;; non-Types don't need masks
           [(not is-a-type?)
            #'(define var.constructor
                (let ([intern-table (make-hash)])
                  (λ (flds.ids ...)
                    (let ([key intern-key]
                          [fail (λ () (let ([fvs free-vars-def]
                                            [fis free-idxs-def])
                                        (var.raw-constructor (count!) fvs fis flds.ids ...)))])
                      (hash-ref! intern-table key fail)))))]
           [else
            ;; Types have to provide Type-masks and subtype caches
            #`(define var.constructor
                (let ([intern-table (make-hash)])
                  (λ (flds.ids ...)
                    (let ([key intern-key]
                          [fail (λ () (let ([fvs free-vars-def]
                                            [fis free-idxs-def]
                                            [mask-val #,(if (attribute type-mask-body)
                                                            #'(let () . type-mask-body)
                                                            #'mask:unknown)])
                                        (var.raw-constructor (count!) fvs fis (make-hash) mask-val flds.ids ...)))])
                      (hash-ref! intern-table key fail)))))])]
        ;; walk def
        [walk-def (cond
                    [(attribute base?) #'(λ (f x) (void))]
                    [else #'walk-spec.def])]
        ;; fold def
        [fold-def (cond
                    [(attribute base?) #'(λ (f x) x)]
                    [else #'fold-spec.def])]
        ;; is this a type that needs resolving (e.g. Mu)
        [(maybe-needs-resolving ...)
         (if (attribute needs-resolving?)
             #'(#:property prop:resolvable #t)
             #'())]
        ;; how do we pull out the values required to fold this Rep?
        [values-def #'(match-lambda
                        [(var.mexpdr flds.ids ...) (list flds.ids ...)])]
        ;; module provided defintions, if any
        [(provides ...)
         (cond
           [(attribute no-provide?) #'()]
           [else
            #'((provide var.mexpdr var.pred flds.accessors ...)
               (provide/cond-contract (var.constructor constructor-contract)))])]
        [(extra-defs ...) (if (attribute extras) #'extras #'())])
       ;; - - - - - - - - - - - - - - -
       ;; macro output
       ;; - - - - - - - - - - - - - - -
       #'(begin
           (struct var.name parent (flds.ids ...) #:transparent
             #:constructor-name
             var.raw-constructor
             #:property prop:Rep-name (quote var.name)
             #:property prop:get-constructor-fun
             (λ (flds.ids ...) (var.constructor flds.ids ...))
             #:property prop:get-values-fun
             values-def
             #:property prop:walk-fun
             walk-def
             #:property prop:fold-fun
             fold-def
             maybe-top-type-spec ...
             maybe-structural ...
             maybe-needs-resolving ...
             extra-defs ...)
           constructor-def
           mexpdr-def
           provides ...))]))


;; macro for easily defining sets of types represented by fixnum bitfields
(define-syntax (define-type-bitfield stx)
  (define-syntax-class atoms-spec
    (pattern [abbrev:id
              name:id
              contract:expr
              predicate:expr]
             #:with bits (format-id #'name "bits:~a" (syntax-e #'name))
             #:with provide #'(provide bits)))
  (define-syntax-class union-spec
    (pattern [abbrev:id
              name:id
              contract:expr
              predicate:expr
              (elements:id ...)
              (~optional (~and #:no-provide no-provide?))]
             #:with bits (format-id #'name "bits:~a" (syntax-e #'name))
             #:with provide #'(provide bits)))
  (syntax-parse stx
    [(_ #:atom-count atomic-type-count:id
        #:atomic-type-vector atomic-type-vector:id
        #:atomic-name-vector atomic-name-vector:id
        #:name-hash name-hash:id
        #:atomic-contract-vector atomic-contract-vector:id
        #:contract-hash contract-hash:id
        #:atomic-predicate-vector atomic-predicate-vector:id
        #:predicate-hash predicate-hash:id
        #:constructor-template (mk-bits:id) mk-expr:expr
        #:atoms
        atoms:atoms-spec ...
        #:unions
        unions:union-spec ...)
     (define max-base-atomic-count 31) ;; this way we can do unsafe fx ops on any machine
     (define atom-list (syntax->datum #'(atoms.name ...)))
     (define atom-count (length atom-list))
     (unless (<= atom-count max-base-atomic-count)
       (raise-syntax-error
        'define-type-bitfield
        (format "too many atomic base types (~a is the max)"
                max-base-atomic-count)
        stx))
     (with-syntax ([(n ... ) (range atom-count)]
                   [(2^n ...)
                    (build-list atom-count (λ (n) (arithmetic-shift 1 n)))])
       #`(begin
           ;; how many atomic types?
           (define atomic-type-count #,atom-count)
           ;; define the atomic types' bit identifiers (e.g. bits:Null)
           (begin (define atoms.bits 2^n) ...)
           ;; define the union types' bit identifiers
           (begin (define unions.bits
                    (bitwise-ior unions.elements ...))
                  ...)
           ;; define the actual type references (e.g. -Null)
           (begin (define/decl atoms.abbrev
                    (let ([mk-bits atoms.bits]) mk-expr)) ...)
           (begin (define/decl unions.abbrev
                    (let ([mk-bits unions.bits]) mk-expr)) ...)
           ;; define the various vectors and hashes
           (define atomic-type-vector
             (vector-immutable atoms.abbrev ...))
           (define atomic-name-vector
             (vector-immutable (quote atoms.name) ...))
           (define name-hash
             (make-immutable-hasheqv
              (list (cons atoms.bits (quote atoms.name)) ...
                    (cons unions.bits (quote unions.name)) ...)))
           (define atomic-contract-vector
             (vector-immutable atoms.contract ...))
           (define contract-hash
             (make-immutable-hasheqv
              (list
               (cons atoms.bits atoms.contract)
              ...
              (cons unions.bits unions.contract)
              ...)))
           (define atomic-predicate-vector
             (vector-immutable atoms.predicate ...))
           (define predicate-hash
             (make-immutable-hasheqv
              (list
               (cons atoms.bits atoms.predicate) ...
               (cons unions.bits unions.predicate) ...)))
           ;; provide the bit variables (e.g. bits:Null)
           atoms.provide ...
           unions.provide ...))]))


(provide
  Rep-values
  (rename-out [Rep-free-vars free-vars*]
              [Rep-free-idxs free-idxs*]))
