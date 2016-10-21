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

;; seq: interning-generated serial number used to compare Reps (type<).
;; free-vars: cached free type variables
;; free-idxs: cached free dot sequence variables
;; stx: originating syntax for error-reporting
(struct Rep (seq) #:transparent
  #:methods gen:equal+hash
  [(define (equal-proc x y recur)
     (unsafe-fx= (Rep-seq x) (Rep-seq y)))
   (define (hash-proc x recur) (Rep-seq x))
   (define (hash2-proc x recur) (Rep-seq x))])

(define (Rep<? x y)
  (unsafe-fx< (Rep-seq x) (Rep-seq y)))

;; definer for struct properties where we can easily omit
;; fields (e.g. the predicate or accessor) when they are not needed
(define-syntax (def-property stx)
  (syntax-parse stx
    [(_ [prop-name predicate? (~datum _)])
     (syntax/loc stx
       (define-values (prop-name predicate?)
         (let-values ([(prop pred _) (make-struct-type-property (quote prop-name))])
           (values prop pred))))]
    [(_ [prop-name (~datum _) accessor-name])
     (syntax/loc stx
       (define-values (prop-name accessor-name)
         (let-values ([(prop _ accessor) (make-struct-type-property (quote prop-name))])
           (values prop accessor))))]
    [(_ [prop-name predicate? accessor-name])
     (syntax/loc stx
       (define-values (prop-name predicate? accessor-name)
         (make-struct-type-property (quote prop-name))))]))

;; definer for struct properties
(define-syntax-rule (def-properties defs ...)
  (begin (def-property defs)
         ...))


(def-properties
  ;; - - - - - - - - - - - -
  ;; Rep Properties
  ;; - - - - - - - - - - - -
  ;; A symbol name for a Rep
  ;; Rep-name : Rep -> symbol
  [prop:Rep-name _ Rep-name]
  ;; an accessor that returns a list of the relevant
  ;; values this Rep contains
  ;; Rep-values-fun : Rep -> (Rep -> (listof any))
  [prop:values-fun _ Rep-values-fun]
  ;; The intended constructor for this Rep.
  ;; i.e. ((Rep-constructor rep) (Rep-values rep)) == rep
  ;; Rep-constructor : Rep -> (any ... -> Rep)
  [prop:constructor-fun _ Rep-constructor]
  ;; can this Rep contain free type variables?
  ;; (i.e. 'F' types from rep/type-rep.rkt)
  ;; Rep-free-ty-vars-fun : Rep -> (#f or (Rep -> (Rep -> free-vars))(
  [prop:free-ty-vars-fun _ Rep-free-ty-vars-fun]
  ;; can this Rep contain free dotted type variables (idxs)?
  ;; (e.g. things like ListDots, etc rep/type-rep.rkt
  ;;  which have an arity/structure which depends on instantiation)
  ;; Rep-free-dotted-ty-vars-fun : Rep -> (#f or (Rep -> (Rep -> free-dotted-vars)))
  [prop:free-ty-idxs-fun _ Rep-free-ty-idxs-fun]
  ;; is this Rep walkable?
  ;; (i.e. can we traverse it w/ some effectful function
  ;;  a lá for-each for lists)
  ;; Rep-walk-fun : Rep -> (#f or (Rep -> (procedure? Rep -> void)))
  [prop:walk-fun _ Rep-walk-fun]
  ;; is this Rep foldable?
  ;; (i.e. can we traverse it w/ applying a function to
  ;;  the fields? a lá map for lists)
  ;; Rep-fold-fun : Rep -> (#f or (Rep -> (procedure? Rep -> void)))
  [prop:fold-fun _ Rep-fold-fun]
  
  ;; - - - - - - - - - - - -
  ;; Type Properties
  ;; - - - - - - - - - - - -
  ;; Is this a type that can be a 'back-edge' into the type graph?
  ;; (i.e. could blindly following this type lead to infinite recursion?)
  ;; Note: 'resolve' is the function which "expands" these types (i.e. follows
  ;; the potentially recursive back edge into the graph)
  [prop:resolvable resolvable? _]
  ;; is this a structural type which has trivial subtyping
  ;; and folding behaviors, assuming we know the variance of the fields
  ;; Type-variances : Type -> (listof Variance?) (see free-variances.rkt)
  [prop:structural structural? Type-variances]
  ;; is there a top type for this type? (e.g. Vector & VectorTop)
  ;; top-type-pred : Type -> (Type -> boolean)
  [prop:top-type has-top-type? Type-top-pred])

;; Rep-values
;; (Rep -> (listof any))
(define (Rep-values rep)
  ((Rep-values-fun rep) rep))

;; Rep-free-vars
;; Rep -> free-vars (see free-variance.rkt)
(define (Rep-free-vars rep)
  (cond [(Rep-free-ty-vars-fun rep)
         => (λ (frees) (frees rep))]
        [else empty-free-vars]))

;; Rep-free-idxs
;; Rep -> free-vars (see free-variance.rkt)
(define (Rep-free-idxs rep)
  (cond [(Rep-free-ty-idxs-fun rep)
         => (λ (frees) (frees rep))]
        [else empty-free-vars]))

;; Rep-walk
;; (Rep -> void) Rep -> void
(define (Rep-walk f rep)
  (cond [(Rep-walk-fun rep)
         => (λ (walk) (walk f rep))]
        [else (void)]))

;; Rep-fold
;; (Rep -> Rep) Rep -> Rep
(define (Rep-fold f rep)
  (cond
    [(Rep-fold-fun rep)
     => (λ (fold) (fold f rep))]
    [else rep]))

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
  ;; defines a rep transformer where the tranforming function is
  ;; a parameter (e.g. free vars)
  (define (rep-transform f-id self fun-name name match-expdr struct-fields body)
    (with-syntax ([f-id f-id]
                  [self self]
                  [fun-name fun-name]
                  [name name]
                  [(flds ...) struct-fields]
                  [mexpdr match-expdr]
                  [body body])
      #'(λ (f-id self)
          (match self
            [(mexpdr flds ...) . body]
            [_ (error 'Rep-walk "bad match in ~a's ~a" (quote name) (quote fun-name))]))))
  ;; defines a rep transformer where the tranforming function is
  ;; known ahead of time (e.g. for free vars, instantiate, etc)
  (define (fixed-rep-transform f-id self fun fun-name name match-expdr struct-fields body)
    (with-syntax ([transformer (rep-transform f-id self fun-name name match-expdr struct-fields body)]
                  [self self]
                  [fun fun])
      #'(λ (self) (transformer fun self))))
  ;; #:frees definition parsing
  (define-syntax-class (freesspec name match-expdr struct-fields)
    #:attributes (free-vars free-idxs)
    (pattern
     ([#:vars (f1 (~optional (~seq #:self self1:id)
                             #:defaults ([self1 (generate-temporary 'self)])))
       . vars-body]
      [#:idxs (f2 (~optional (~seq #:self self2:id)
                             #:defaults ([self2 (generate-temporary 'self)])))
       . idxs-body])
     #:with free-vars
     (fixed-rep-transform #'f1 #'self1 #'Rep-free-vars #'free-vars name match-expdr struct-fields #'vars-body)
     #:with free-idxs
     (fixed-rep-transform #'f2 #'self2 #'Rep-free-idxs #'free-idxs name match-expdr struct-fields #'idxs-body))
    (pattern ((f:id (~optional (~seq #:self self:id)
                               #:defaults ([self (generate-temporary 'self)])))
              . body)
             #:with free-vars
             (fixed-rep-transform #'f #'self #'Rep-free-vars #'free-vars name match-expdr struct-fields #'body)
             #:with free-idxs
             (fixed-rep-transform #'f #'self #'Rep-free-idxs #'free-vars name match-expdr struct-fields #'body)))
  ;; definer parser for functions who operate on Reps. Fields are automatically bound
  ;; to the struct-field id names in the body. An optional self argument can be specified.
  (define-syntax-class (generic-transformer op-name name match-expdr struct-fields)
    #:attributes (def)
    (pattern ((f:id (~optional (~seq #:self self:id)
                               #:defaults ([self (generate-temporary 'self)])))
              . body)
             #:with def
             (rep-transform #'f #'self op-name name match-expdr struct-fields #'body)))
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
       (~optional [#:frees . (~var frees-spec (freesspec #'var.name
                                                         #'var.mexpdr
                                                         #'(flds.ids ...)))])
       ;; #:walk spec (how to traverse this structure for effect)
       (~optional [#:walk . (~var walk-spec (generic-transformer #'walk
                                                                 #'var.name
                                                                 #'var.mexpdr
                                                                 #'(flds.ids ...)))])
       ;; #:fold spec (how to transform & fold this structure)
       (~optional [#:fold . (~var fold-spec (generic-transformer #'fold
                                                                 #'var.name
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
     ;; no frees, walk, fold, or abs/inst for #:base Reps
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
                        #'(syntax/loc s (var.name _ _ _ . pats))
                        #'(syntax/loc s (var.name _ . pats)))])))]
        ;; free var/idx defs
        [free-vars-def (cond
                         [(attribute base?) #'#f]
                         [else #'frees-spec.free-vars])]
        [free-idxs-def (cond
                         [(attribute base?) #'#f]
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
                          [fail (λ () (var.raw-constructor (count!) flds.ids ...))])
                      (hash-ref! intern-table key fail)))))]
           [else
            ;; Types have to provide Type-masks and subtype caches
            #`(define var.constructor
                (let ([intern-table (make-hash)])
                  (λ (flds.ids ...)
                    (let ([key intern-key]
                          [fail (λ () (let ([mask-val #,(if (attribute type-mask-body)
                                                            #'(let () . type-mask-body)
                                                            #'mask:unknown)])
                                        (var.raw-constructor (count!) (make-hash) mask-val flds.ids ...)))])
                      (hash-ref! intern-table key fail)))))])]
        ;; walk def
        [walk-def (cond
                    [(attribute base?) #'#f]
                    [else #'walk-spec.def])]
        ;; fold def
        [fold-def (cond
                    [(attribute base?) #'#f]
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
             #:property prop:constructor-fun (λ (flds.ids ...) (var.constructor flds.ids ...))
             #:property prop:values-fun values-def
             #:property prop:walk-fun walk-def
             #:property prop:fold-fun fold-def
             #:property prop:free-ty-vars-fun free-vars-def
             #:property prop:free-ty-idxs-fun free-idxs-def
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
