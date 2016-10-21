#lang racket/base
(require "../utils/utils.rkt"
         (utils struct-interface tc-utils)
         
         racket/match
         (contract-req)
         "free-variance.rkt"
         "type-mask.rkt"
         racket/stxparam
         syntax/parse/define
         syntax/id-table
         racket/generic
         (only-in racket/unsafe/ops unsafe-struct-ref)
         (for-syntax
          (utils tc-utils)
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
         get-uid-count
         (for-syntax var-name))


;; Contract and Hash related helpers

(provide-for-cond-contract length>=/c)

(define-for-cond-contract ((length>=/c len) l)
  (and (list? l)
       (>= (length l) len)))

(define-for-cond-contract name-ref/c
  (or/c identifier?
        (cons/c exact-integer? exact-integer?)))

(define (name-ref=? x y)
  (cond
    [(identifier? x)
     (and (identifier? y) (free-identifier=? x y))]
    [else (equal? x y)]))

(define id-table (make-free-id-table))
(define (normalize-id id)
  (free-id-table-ref! id-table id id))

(define (hash-id id)
  (eq-hash-code (identifier-binding-symbol id)))

(define (hash-name-ref name rec)
  (if (identifier? name)
      (hash-id name)
      (rec name)))


;; This table maps types (or really, the sequence number of the type)
;; to identifiers that are those types. This allows us to avoid
;; reconstructing the type when using it from its marshaled
;; representation.  The table is referenced in env/init-env.rkt
;;
;; For example, instead of marshalling a big union for `Integer`, we
;; simply emit `-Integer`, which evaluates to the right type.
(define predefined-type-table (make-hash))
(define-syntax-rule (declare-predefined-type! id)
  (hash-set! predefined-type-table id #'id))
(provide predefined-type-table)
(define-syntax-rule (define/decl id e)
  (begin (define id e)
         (declare-predefined-type! id)))


;; - - - - - - - - - - - -
;; Rep Properties
;; - - - - - - - - - - - -
(define-generics Rep
  ;; A symbol name for a Rep
  ;; Rep-name : Rep -> symbol
  (Rep-name Rep)
  ;; The values this rep contains (see Rep-constructor).
  ;; Rep-values : Rep -> (listof any/c)
  (Rep-values Rep)
  ;; is there a simple, structural description for the variances
  ;; of this Rep's fields? (currently only used for 'structural' types,
  ;; see type-rep.rkt
  ;; Rep-constructor : Rep -> (any ... -> Rep)
  (Rep-variances Rep)
  ;; The intended constructor for this Rep.
  ;; i.e. (equal? ((Rep-constructor rep) (Rep-values rep)) rep) = #t
  ;; Rep-constructor : Rep -> (any ... -> Rep)
  (Rep-constructor Rep)
  ;; can this Rep contain free type variables?
  ;; (i.e. 'F' types from rep/type-rep.rkt)
  ;; Rep-free-ty-vars-fun : Rep -> free-vars
  (Rep-free-vars Rep)
  ;; can this Rep contain free dotted type variables (idxs)?
  ;; (e.g. things like ListDots, etc rep/type-rep.rkt
  ;;  which have an arity/structure which depends on instantiation)
  ;; Rep-free-ty-idxs-fun : Rep -> free-dotted-vars
  (Rep-free-idxs Rep)
  ;; is this Rep mappable?
  ;; (i.e. can we traverse it w/ applying a function to
  ;;  the fields? a lá map for lists)
  ;; Rep-fmap : Rep procedure -> Rep
  (Rep-fmap Rep f)
  ;; is this Rep walkable?
  ;; (i.e. can we traverse it w/ some effectful function
  ;;  a lá for-each for lists)
  ;; Rep-walk-fun : Rep procedure -> void
  (Rep-for-each Rep f))

;; used internally when generating gen:Rep method definitions
;; so that we don't have to mess around w/ 'define/generic'
(define-syntax free-vars* (make-rename-transformer #'Rep-free-vars))
(define-syntax free-idxs* (make-rename-transformer #'Rep-free-idxs))

;; A variant-unique fixnum.
;; Rep-uid : Rep -> fixnum
(define-values (prop:uid Rep-uid)
  (let-values ([(prop _ acc) (make-struct-type-property 'uid)])
    (values prop acc)))


(define-values (prop:mask raw-mask)
  (let-values ([(prop _ acc) (make-struct-type-property 'mask)])
    (values prop acc)))

;; Type-mask : Rep -> fixnum
(define-syntax-rule (mask rep)
  (let ([mask (raw-mask rep)])
    (if (procedure? mask)
        (mask rep)
        mask)))


;;************************************************************
;; Rep Declaration Syntax Classes
;;************************************************************
(define-values (next-uid! get-uid-count)
  (let ([state 0]
        [finalized? #f])
    (values (λ () (if finalized?
                      (int-err "next-uid! called after uid count finalized!")
                      (begin0 state (set! state (add1 state)))))
            (λ () (set! finalized? #t) state))))

(define-syntax (define-switch stx)
  (define-syntax-class (switch-clause arg other-args)
    (pattern (((~datum case:) rep-name:id pattern:expr) . body)
             #:with name #'rep-name
             #:with idx (format-id #'rep-name "uid:~a" (syntax->datum #'rep-name))
             #:with function
             (with-syntax ([arg arg]
                           [other-args other-args])
               (syntax/loc #'body
                 (λ (arg . other-args)
                   (match arg
                     [pattern . body]))))))
  (syntax-parse stx
    [(_ (name:id arg:id args:id ...)
        (~var clause (switch-clause #'arg #'(args ...))) ...
        [(~datum else:) . default])
     (define name-symbols (map syntax->datum (syntax->list #'(clause.name ...))))
     (unless (not (null? name-symbols))
       (raise-syntax-error 'define-switch "switch cannot be null" stx))
     (define sorted-name-symbols (sort name-symbols symbol<?))
     (unless (eq? (first name-symbols) (first sorted-name-symbols))
       (raise-syntax-error 'define-switch
                           (format "expected ~a as the first case"
                                   (first sorted-name-symbols))
                           stx))
     ;; we verify that the Rep cases appear in name-sorted order (so
     ;; they are easier to find when browsing the code) and that there
     ;; are no duplicate cases
     (for ([cur (in-list (rest name-symbols))]
           [cur-stx (in-list (rest (syntax->list #'(clause ...))))]
           [cur* (in-list (rest sorted-name-symbols))]
           [prev* (in-list sorted-name-symbols)]
           [prev-stx (in-list (syntax->list #'(clause ...)))])
       (when (eq? cur* prev*)
         (raise-syntax-error 'define-switch
                             (format "duplicate switch cases for ~a" prev*)
                             prev-stx))
       (unless (eq? cur cur*)
         (raise-syntax-error 'define-switch
                             (format "switch cases must be sorted! expected ~a but got ~a"
                                     cur* cur)
                             cur-stx)))
     (syntax/loc stx
       (define name
         (let ([switch-table (make-vector (get-uid-count) (λ (arg args ...) . default))])
           (vector-set! switch-table clause.idx clause.function)
           ...
           (λ (arg args ...) ((vector-ref switch-table (Rep-uid arg)) arg args ...)))))]))


(begin-for-syntax
  ;; defines a rep transformer where the tranforming function is
  ;; a parameter (e.g. free vars)
  (define (rep-transform self f-id struct-fields body)
    (with-syntax ([f-id f-id]
                  [self self]
                  [(fld ...) struct-fields]
                  [body body])
      #'(λ (self f-id)
          (let ([fld (unsafe-struct-ref self (struct-field-index fld))] ...) . body))))
  ;; defines a rep transformer where the tranforming function is
  ;; known ahead of time (e.g. for free vars, instantiate, etc)
  (define (fixed-rep-transform self f-id fun struct-fields body)
    (with-syntax ([transformer (rep-transform self f-id struct-fields body)]
                  [self self]
                  [fun fun])
      #'(λ (self) (transformer self fun))))
  ;; #:frees definition parsing
  (define-syntax-class (freesspec struct-fields)
    #:attributes (free-vars free-idxs)
    (pattern
     ([#:vars (f1 (~optional (~seq #:self self1:id)
                             #:defaults ([self1 (generate-temporary 'self)])))
       . vars-body]
      [#:idxs (f2 (~optional (~seq #:self self2:id)
                             #:defaults ([self2 (generate-temporary 'self)])))
       . idxs-body])
     #:with free-vars (fixed-rep-transform #'self1 #'f1 #'free-vars* struct-fields #'vars-body)
     #:with free-idxs (fixed-rep-transform #'self2 #'f2 #'free-idxs* struct-fields #'idxs-body))
    (pattern
     ((f:id (~optional (~seq #:self self:id)
                       #:defaults ([self (generate-temporary 'self)])))
      . body)
     #:with free-vars (fixed-rep-transform #'self #'f #'free-vars* struct-fields #'body)
     #:with free-idxs (fixed-rep-transform #'self #'f #'free-idxs* struct-fields #'body)))
  (define-syntax-class (constructor-spec constructor-name raw-constructor-name struct-fields)
    #:attributes (def)
    (pattern body
             #:with def
             (with-syntax ([constructor-name constructor-name]
                           [raw-constructor-name raw-constructor-name]
                           [(struct-fields ...) struct-fields])
               #'(define (constructor-name struct-fields ...)
                   (let ([constructor-name raw-constructor-name])
                     . body)))))
  ;; definer parser for functions who operate on Reps. Fields are automatically bound
  ;; to the struct-field id names in the body. An optional self argument can be specified.
  (define-syntax-class (generic-transformer struct-fields)
    #:attributes (def)
    (pattern ((f:id (~optional (~seq #:self self:id)
                               #:defaults ([self (generate-temporary 'self)])))
              . body)
             #:with def (rep-transform #'self #'f struct-fields #'body)))
  ;; variant name parsing
  (define-syntax-class var-name
    #:attributes (name constructor raw-constructor match-expander predicate)
    (pattern name:id
             #:with constructor
             (format-id #'name "make-~a" (syntax-e #'name))
             ;; hidden constructor for use inside custom constructor defs
             #:with raw-constructor (format-id #'name "raw-make-~a" (syntax-e #'name))
             #:with match-expander
             (format-id #'name "~a:" (syntax-e #'name))
             #:with predicate
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
       (~optional [#:parent parent:id])
       ;; base declaration (i.e. no fold/map)
       (~optional (~and #:base base?))
       ;; #:frees spec (how to compute this Rep's free type variables)
       (~optional [#:frees . (~var frees-spec (freesspec #'(flds.ids ...)))])
       ;; #:for-each spec (how to traverse this structure for effect)
       (~optional [#:for-each . (~var for-each-spec (generic-transformer #'(flds.ids ...)))])
       ;; #:fold spec (how to transform & fold this structure)
       (~optional [#:fmap . (~var fold-spec (generic-transformer #'(flds.ids ...)))])
       (~optional [#:mask . rep-mask-body])
       (~optional [#:variances ((~literal list) variances ...)])
       ;; #:no-provide option (i.e. don't provide anything automatically)
       (~optional (~and #:no-provide no-provide?-kw))
       (~optional [#:singleton singleton:id])
       (~optional [#:custom-constructor . (~var constr-def
                                                (constructor-spec #'var.constructor
                                                                  #'var.raw-constructor
                                                                  #'(flds.ids ...)))])
       (~optional (~and #:non-transparent non-transparent-kw))
       ;; #:extras to specify other struct properties in a per-definition manner
       (~optional [#:extras . extras]))
      ...)

     ;; - - - - - - - - - - - - - - -
     ;; Error checking
     ;; - - - - - - - - - - - - - - -
     
     ;; build convenient boolean flags
     (define is-a-type? (and (attribute parent) (eq? 'Type (syntax-e #'parent))))
     ;; singletons cannot have fields or #:no-provide
     (when (and (attribute singleton)
                (or (attribute no-provide?-kw)
                    (> (length (syntax->list #'flds)) 0)))
       (raise-syntax-error 'def-rep "singletons cannot have fields or the #:no-provide option"
                           #'var))
     (when (and (attribute base?)
                (attribute singleton))
       (raise-syntax-error 'def-rep "singletons are base by def, do not provide #:base option"
                           #'var))
     ;; no frees, for-each, fold, or abs/inst for #:base Reps
     (when (and (or (attribute base?)
                    (attribute singleton))
                (or (attribute frees-spec)
                    (attribute for-each-spec)
                    (attribute fold-spec)))
       (raise-syntax-error 'def-rep "base reps and singletons cannot have #:frees, #:for-each, or #:fold"
                           #'var))
     ;; if non-base, frees, for-each, and fold are required
     (when (and (not (or (attribute base?) (attribute singleton)))
                (or (not (attribute frees-spec))
                    (not (attribute for-each-spec))
                    (not (attribute fold-spec))))
       (raise-syntax-error 'def-rep "non-base reps require #:frees, #:for-each, and #:fold"
                           #'var))

     ;; - - - - - - - - - - - - - - -
     ;; Let's build the definitions!
     ;; - - - - - - - - - - - - - - -
     (with-syntax*
       ([uid-id (format-id #'var.name "uid:~a" (syntax->datum #'var.name))]
        [(parent ...) (if (attribute parent) #'(parent) #'())]
        ;; contract for constructor
        [constructor-contract #'(-> flds.contracts ... var.predicate)]
        [constructor-name (if (attribute constr-def)
                              #'var.raw-constructor
                              #'var.constructor)]
        [constructor-def (if (attribute constr-def)
                             #'constr-def.def
                             #'(begin))]
        [(maybe-transparent ...) (if (attribute non-transparent-kw)
                                     #'()
                                     #'(#:transparent))]
        ;; match expander (skips 'meta' fields)
        [mexpdr-def
         #`(define-match-expander var.match-expander
             (λ (s)
               (syntax-parse s
                 [(_ . pats) (syntax/loc s (var.name . pats))])))]
        ;; Rep generic definitions
        ;; -----------------------
        ;; free var/idx defs
        [Rep-name-def
         #'(define (Rep-name _) 'var.name)]
        [Rep-values-def
         #'(define (Rep-values rep)
             (match rep
               [(var.name flds.ids ...) (list flds.ids ...)]))]
        [Rep-variances-def
         (cond
           [(attribute variances)
            #'(define (Rep-variances _)
                (list variances ...))]
           [else
            #'(define (Rep-variances _) #f)])]
        [Rep-constructor-def
         #'(define (Rep-constructor rep) var.constructor)]
        ;; free var/idx defs
        [Rep-free-vars-def
         (cond
           [(or (attribute base?)
                (attribute singleton))
            #'(define (Rep-free-vars _) empty-free-vars)]
           [else #'(define Rep-free-vars frees-spec.free-vars)])]
        [Rep-free-idxs-def
         (cond
           [(or (attribute base?)
                (attribute singleton))
            #'(define (Rep-free-idxs _) empty-free-vars)]
           [else #'(define Rep-free-idxs frees-spec.free-idxs)])]
        ;; for-each def
        [Rep-for-each-def
         (cond
           [(or (attribute base?) (attribute singleton))
            #'(define (Rep-for-each rep f) (void))]
           [else #'(define Rep-for-each for-each-spec.def)])]
        ;; fold def
        [Rep-fmap-def
         (cond
           [(or (attribute base?) (attribute singleton))
            #'(define (Rep-fmap rep f) rep)]
           [else #'(define Rep-fmap fold-spec.def)])]
        ;; how do we pull out the values required to fold this Rep?
        [rep-mask-body 
         (cond
           [(attribute rep-mask-body) #'(let () . rep-mask-body)]
           [else #'mask:unknown])]
        ;; module provided defintions, if any
        [(provides ...)
         (cond
           [(attribute no-provide?-kw) #'()]
           [else
            #'((provide var.match-expander var.predicate flds.accessors ...)
               (provide/cond-contract (var.constructor constructor-contract)))])]
        [(extra-defs ...) (if (attribute extras) #'extras #'())]
        [struct-def #'(struct var.name parent ... (flds.ids ...)
                        maybe-transparent ...
                        #:constructor-name constructor-name
                        #:property prop:uid uid-id
                        #:property prop:mask rep-mask-body
                        #:methods gen:Rep
                        [Rep-name-def
                         Rep-values-def
                         Rep-constructor-def
                         Rep-variances-def
                         Rep-free-vars-def
                         Rep-free-idxs-def
                         Rep-for-each-def
                         Rep-fmap-def]
                        extra-defs ...)])
       ;; - - - - - - - - - - - - - - -
       ;; macro output
       ;; - - - - - - - - - - - - - - -
       (cond
         [(attribute singleton)
          (syntax/loc stx
            (begin
              (define uid-id (next-uid!))
              (define singleton
                (let ()
                  struct-def
                  (var.constructor)))
              (declare-predefined-type! singleton)
              (define (var.predicate x) (eq? x singleton))
              (define-match-expander var.match-expander
                (λ (s)
                  (syntax-parse s
                    [(_) (syntax/loc s (? var.predicate))])))
              (provide singleton var.predicate var.match-expander
                       uid-id)))]
         [else
          (syntax/loc stx
            (begin
              (define uid-id (next-uid!))
              struct-def
              constructor-def
              mexpdr-def
              provides ...
              (provide uid-id)))]))]))


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
