#lang racket/base
(require "../utils/utils.rkt"
         (utils tc-utils identifier)
         
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
          (except-in syntax/parse id identifier keyword)
          racket/base
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

(define (name-ref/c x)
  (or (identifier? x)
      (and (pair? x)
           (exact-integer? (car x))
           (exact-integer? (cdr x)))))

(define (name-ref=? x y)
  (cond
    [(identifier? x)
     (and (identifier? y) (free-identifier=? x y))]
    [else (equal? x y)]))

;; normal-id-table
;;
;; id table to map all free-identifier=? ids from
;; the users program to the same id for typechecking
;; purposes (this gives us equal?, which is really nice.
;; the alternative is implementing gen:equal-hash for all
;; the structs that carry identifiers, but this was noticably
;; slower when I tried it out in late 2016 -amk)
;; Note: we use a free-id-table, which could _potentially_
;; leak memory if we're encountering lots of identifiers
;; that shouldn't stay in memory for the entirety of typechecking,
;; HOWEVER, we don't put our gensymed identifiers into this table
;; (they are 'normal' from the get-go -- see utils/utils.rkt),
;; so this shouldnt be an issue, we should just be putting
;; identifiers from the program itself in here, and those
;; will be in memory for typechecking anyway.
(define normal-id-table (make-free-id-table))

;; gets the "canonical" representative for this id.
;; if one does not exist yet, the id is marked
(define (normalize-id id)
  (cond
    ;; if it's alread normal, just return it
    [(normalized-id? id) id]
    ;; otherwise check if id-table has the normal
    ;; version already and if so, return that
    [(free-id-table-ref normal-id-table id #f)]
    [else
     ;; otherwise mark this id as normal, put it
     ;; in the table, and return it (this is now
     ;; the canonical id for all other ids
     ;; free-identifier=? to this one
     (let ([id (mark-id-as-normalized id)])
       (free-id-table-set! normal-id-table id id)
       id)]))

(define (hash-id id)
  (eq-hash-code (identifier-binding-symbol id)))

(define (hash-name-ref name rec)
  (if (identifier? name)
      (hash-id name)
      (rec name)))

;; This table maps a type to an identifier bound to the type.
;; This allows us to avoid reconstructing the type when using
;; it from its marshaled representation.
;; The table is referenced in env/init-env.rkt
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


;; fetches an interned Rep based on the given _single_ key
;; NOTE: the #:construct expression is only run if there
;; is no interned copy, so we should avoid unnecessary
;; allocation w/ this approach
(define-simple-macro (intern-single-ref! table-exp:expr
                                         key-exp:expr
                                         #:construct val-exp:expr)
  (let ([table table-exp])
    (define key key-exp)
    (define intern-box (hash-ref table key #f))
    (cond
      [(and intern-box (weak-box-value intern-box #f))]
      [else
       (define val val-exp)
       (hash-set! table key (make-weak-box val))
       val])))

;; fetches an interned Rep based on the given _two_ keys
;; see 'intern-single-ref!'
(define-simple-macro (intern-double-ref! table:id
                                         key-exp1:expr
                                         key-exp2:expr
                                         #:construct val-exp:expr)
  (intern-single-ref! (hash-ref! table key-exp1 make-hash)
                      key-exp2
                      #:construct val-exp))



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

(begin-for-syntax
  ;; defines a "rep transformer"
  ;; These are functions defined to fold over Reps (e.g. Rep-fmap, Rep-for-each).
  ;; Because they are defined within the definition of the struct, they bind the same
  ;; identifiers which declare the Rep's fields to those same fields.
  (define (rep-transform self f-id struct-fields body)
    (with-syntax ([f-id f-id]
                  [self self]
                  [(fld ...) struct-fields]
                  [body body])
      #'(λ (self f-id)
          (let ([fld (unsafe-struct-ref self (struct-field-index fld))] ...) . body))))
  ;; like "rep-transform" but the folding function is fixed (e.g. free-vars)
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
  (define-syntax-class (constructor-spec constructor-name
                                         constructor-contract
                                         raw-constructor-name
                                         raw-constructor-contract
                                         struct-fields)
    #:attributes (def)
    (pattern body
             #:with def
             (with-syntax ([constructor-name constructor-name]
                           [constructor-contract constructor-contract]
                           [raw-constructor-name raw-constructor-name]
                           [raw-constructor-contract raw-constructor-contract]
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
             #:with constructor (format-id #'name "make-~a" (syntax-e #'name))
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
       (~optional (~or [#:custom-constructor
                        . (~var constr-def
                                (constructor-spec #'var.constructor
                                                  #'(-> flds.contracts ... any)
                                                  #'var.raw-constructor
                                                  #'(-> flds.contracts ... any)
                                                  #'(flds.ids ...)))]
                       [#:custom-constructor/contract
                        custom-constructor-contract
                        . (~var constr-def
                                (constructor-spec #'var.constructor
                                                  #'custom-constructor-contract
                                                  #'var.raw-constructor
                                                  #'(-> flds.contracts ... any)
                                                  #'(flds.ids ...)))]))
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
       (raise-syntax-error 'def-rep "non-base reps require #:frees, #:for-each, and #:fmap"
                           #'var))

     ;; - - - - - - - - - - - - - - -
     ;; Let's build the definitions!
     ;; - - - - - - - - - - - - - - -
     (with-syntax*
       ([uid-id (format-id #'var.name "uid:~a" (syntax->datum #'var.name))]
        [(parent ...) (if (attribute parent) #'(parent) #'())]
        ;; contract for constructor
        [constructor-contract (if (attribute custom-constructor-contract)
                                  #'custom-constructor-contract
                                  #'(-> flds.contracts ... any))]
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
