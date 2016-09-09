#lang racket/base

;; This file is for the abbreviations needed to implement union.rkt
;;
;; The "abbrev.rkt" module imports this module, re-exports it, and
;; extends it with more types and type abbreviations.

(require "../utils/utils.rkt"
         (rep type-rep prop-rep object-rep values-rep rep-utils)
         (env mvar-env)
         racket/match racket/list (prefix-in c: (contract-req))
         (for-syntax racket/base syntax/parse racket/list)
         ;; For contract predicates
         (for-template racket/base))

(provide (all-defined-out)
         -is-type
         -not-type
         -id-path
         (rename-out [make-Listof -lst]
                     [make-MListof -mlst]))

;; This table maps types (or really, the sequence number of the type)
;; to identifiers that are those types. This allows us to avoid
;; reconstructing the type when using it from its marshaled
;; representation.  The table is referenced in env/init-env.rkt
;;
;; For example, instead of marshalling a big union for `Integer`, we
;; simply emit `-Integer`, which evaluates to the right type.
(define predefined-type-table (make-hasheq))
(define-syntax-rule (declare-predefined-type! id)
  (hash-set! predefined-type-table (Rep-seq id) #'id))
(provide predefined-type-table)
(define-syntax-rule (define/decl id e)
  (begin (define id e)
	 (declare-predefined-type! id)))

;; Top and error types
(define/decl Univ (make-Univ))
(define/decl -Bottom (make-Bottom))
(define/decl Err (make-Error))

(define/decl -False (make-Value #f))
(define/decl -True (make-Value #t))
(define/decl -Boolean (make-Union (list -False -True)))

(define -val make-Value)

;; Char type and List type (needed because of how sequences are checked in subtype)
(define/decl -Char (make-Base 'Char #'char? char? #f))
(define/decl -Null (-val null))
(define (make-Listof elem) (-mu list-rec (simple-Un -Null (make-Pair elem list-rec))))
(define (make-MListof elem) (-mu list-rec (simple-Un -Null (make-MPair elem list-rec))))

;; Needed for evt checking in subtype.rkt
(define/decl -Symbol (make-Base 'Symbol #'symbol? symbol? #f))
(define/decl -String (make-Base 'String #'string? string? #f))

;; Void is needed for Params
(define/decl -Void (make-Base 'Void #'void? void? #f))

;; -Tuple Type is needed by substitute for ListDots
(define -pair make-Pair)
(define (-lst* #:tail [tail -Null] . args)
  (for/fold ([tl tail]) ([a (in-list (reverse args))]) (-pair a tl)))
(define (-Tuple l)
  (-Tuple* l -Null))
(define (-Tuple* l b)
  (foldr -pair b l))

;; Simple union type constructor, does not check for overlaps
;; Normalizes representation by sorting types.
;; Type * -> Type
;; The input types can be union types, but should not have a complicated
;; overlap relationship.
(define simple-Un
  (let ([flat (match-lambda
                [(Union: es) es]
                [t (list t)])])
    (case-lambda
      [() -Bottom]
      [(t) t]
      [args
       (make-Union (remove-duplicates (append-map flat args) type-equal?))])))

;; Recursive types
(define-syntax -v
  (syntax-rules ()
    [(_ x) (make-F 'x)]))

(define-syntax -mu
  (syntax-rules ()
    [(_ var ty)
     (let ([var (-v var)])
       (make-Mu 'var ty))]))

;; Results
(define/cond-contract (-result t [pset -tt-propset] [o -empty-obj])
  (c:->* (Type?) (PropSet? OptObject?) Result?)
  (cond
    [(or (equal? t -Bottom) (equal? pset -ff-propset))
     (make-Result -Bottom -ff-propset o)]
    [else
     (make-Result t pset o)]))

;; Propositions
(define/decl -tt (make-TrueProp))
(define/decl -ff (make-FalseProp))
(define/decl -tt-propset (make-PropSet -tt -tt))
(define/decl -ff-propset (make-PropSet -ff -ff))
(define/decl -empty-obj (make-Empty))

(define (-arg-path arg [depth 0])
  (make-Path null (cons depth arg)))
(define (-acc-path path-elems o)
  (match o
    [(Empty:) -empty-obj]
    [(Path: p o) (make-Path (append path-elems p) o)]))

(define/cond-contract (-PS + -)
  (c:-> Prop? Prop? PropSet?)
  (make-PropSet + -))


;; A Type that corresponds to the any contract for the
;; return type of functions
(define (-AnyValues f) (make-AnyValues f))
(define/decl ManyUniv (make-AnyValues -tt))

;; Function types
(define/cond-contract (make-arr* dom rng
                                 #:rest [rest #f] #:drest [drest #f] #:kws [kws null]
                                 #:props [props -tt-propset] #:object [obj -empty-obj])
  (c:->* ((c:listof Type?) (c:or/c SomeValues? Type?))
         (#:rest (c:or/c #f Type?)
          #:drest (c:or/c #f (c:cons/c Type? symbol?))
          #:kws (c:listof Keyword?)
          #:props PropSet?
          #:object OptObject?)
         arr?)
  (make-arr dom (if (Type? rng)
                    (make-Values (list (-result rng props obj)))
                    rng)
            rest drest (sort #:key Keyword-kw kws keyword<?)))

(define-syntax (->* stx)
  (define-syntax-class c
    (pattern x:id #:fail-unless (eq? ': (syntax-e #'x)) #f))
  (syntax-parse stx
    [(_ dom rng)
     (syntax/loc stx
       (make-Function (list (make-arr* dom rng))))]
    [(_ dom rst rng)
     (syntax/loc stx
       (make-Function (list (make-arr* dom rng #:rest rst))))]
    [(_ dom rng :c props)
     (syntax/loc stx
       (make-Function (list (make-arr* dom rng #:props props))))]
    [(_ dom rng _:c props _:c object)
     (syntax/loc stx
       (make-Function (list (make-arr* dom rng #:props props #:object object))))]
    [(_ dom rst rng _:c props)
     (syntax/loc stx
       (make-Function (list (make-arr* dom rng #:rest rst #:props props))))]
    [(_ dom rst rng _:c props : object)
     (syntax/loc stx
       (make-Function (list (make-arr* dom rng #:rest rst #:props props #:object object))))]))

(define-syntax (-> stx)
  (define-syntax-class c
    (pattern x:id #:fail-unless (eq? ': (syntax-e #'x)) #f))
  (syntax-parse stx
    [(_ dom ... rng _:c props _:c objects)
     (syntax/loc stx
       (->* (list dom ...) rng : props : objects))]
    [(_ dom ... rng :c props)
     (syntax/loc stx
       (->* (list dom ...) rng : props))]
    [(_ dom ... rng)
     (syntax/loc stx
       (->* (list dom ...) rng))]))

(define-syntax (->... stx)
  (syntax-parse stx
    [(_ dom rng) (syntax/loc stx (->* dom rng))]
    [(_ dom (dty dbound) rng)
     (syntax/loc stx
       (make-Function (list (make-arr* dom rng #:drest (cons dty 'dbound)))))]
    [(_ dom rng (~datum :) props)
     (syntax/loc stx
       (->* dom rng (~datum :) props))]
    [(_ dom (dty dbound) rng (~datum :) props)
     (syntax/loc stx
       (make-Function (list (make-arr* dom rng #:drest (cons dty 'dbound) #:props props))))]))

(define (simple-> doms rng)
  (->* doms rng))

(define (->acc dom rng path #:var [var (cons 0 0)])
  (define obj (-acc-path path (-id-path var)))
  (make-Function
   (list (make-arr* dom rng
                    #:props (-PS (-not-type obj (-val #f))
                                 (-is-type obj (-val #f)))
                    #:object obj))))

(define (cl->* . args)
  (define (funty-arities f)
    (match f
      [(Function: as) as]))
  (make-Function (apply append (map funty-arities args))))

(define-syntax (cl-> stx)
  (syntax-parse stx
    [(_ [(dom ...) rng] ...)
     (syntax/loc stx
       (cl->* (dom ... . -> . rng) ...))]))

(define-syntax (->key stx)
  (syntax-parse stx
    [(_ ty:expr ... (~seq k:keyword kty:expr opt:boolean) ... rng)
     (syntax/loc stx
       (make-Function
        (list
         (make-arr* (list ty ...)
                    rng
                    #:kws (sort #:key (match-lambda [(Keyword: kw _ _) kw])
                                (list (make-Keyword 'k kty opt) ...)
                                keyword<?)))))]))

(define-syntax (->optkey stx)
  (syntax-parse stx
    [(_ ty:expr ... [oty:expr ...] #:rest rst:expr (~seq k:keyword kty:expr opt:boolean) ... rng)
     (let ([l (syntax->list #'(oty ...))])
       (with-syntax ([((extra ...) ...)
                      (for/list ([i (in-range (add1 (length l)))])
                        (take l i))]
                     [(rsts ...) (for/list ([i (in-range (add1 (length l)))]) #'rst)])
         (syntax/loc stx
           (make-Function
            (list
             (make-arr* (list ty ... extra ...)
                        rng
                        #:rest rsts
                        #:kws (sort #:key (match-lambda [(Keyword: kw _ _) kw])
                                    (list (make-Keyword 'k kty opt) ...)
                                    keyword<?))
             ...)))))]
    [(_ ty:expr ... [oty:expr ...] (~seq k:keyword kty:expr opt:boolean) ... rng)
     (let ([l (syntax->list #'(oty ...))])
       (with-syntax ([((extra ...) ...)
                      (for/list ([i (in-range (add1 (length l)))])
                        (take l i))])
         (syntax/loc stx
           (make-Function
            (list
             (make-arr* (list ty ... extra ...)
                        rng
                        #:rest #f
                        #:kws (sort #:key (match-lambda [(Keyword: kw _ _) kw])
                                    (list (make-Keyword 'k kty opt) ...)
                                    keyword<?))
             ...)))))]))

(define (make-arr-dots dom rng dty dbound)
  (make-arr* dom rng #:drest (cons dty dbound)))

;; Convenient syntax for polymorphic types
(define-syntax -poly
  (syntax-rules ()
    [(_ (vars ...) ty)
     (let ([vars (-v vars)] ...)
       (make-Poly (list 'vars ...) ty))]))

(define-syntax -polydots
  (syntax-rules ()
    [(_ (vars ... dotted) ty)
     (let ([dotted (-v dotted)]
           [vars (-v vars)] ...)
       (make-PolyDots (list 'vars ... 'dotted) ty))]))

(define-syntax -polyrow
  (syntax-rules ()
    [(_ (var) consts ty)
     (let ([var (-v var)])
       (make-PolyRow (list 'var) consts ty))]))

