#lang racket/base

;; This module provides type representations and utility functions
;; and pattern matchers on types

(require "../utils/utils.rkt"
         (for-syntax "../utils/utils.rkt"))

;; TODO use contract-req
(require (utils tc-utils prefab identifier)
         "rep-utils.rkt"
         "core-rep.rkt"
         "object-rep.rkt"
         "prop-rep.rkt"
         "values-rep.rkt"
         "type-mask.rkt"
         "free-variance.rkt"
         "base-type-rep.rkt"
         "base-types.rkt"
         "numeric-base-types.rkt"
         "base-union.rkt"
         racket/match racket/list
         syntax/id-table
         racket/contract
         racket/lazy-require
         racket/unsafe/undefined
         (for-syntax racket/base
                     racket/syntax
                     syntax/parse))

(provide (except-out (all-from-out "core-rep.rkt"
                                   "base-type-rep.rkt"
                                   "base-union.rkt")
                     Type Prop Object PathElem SomeValues)
         Type?
         Mu-maybe-name:
         Vector: Vector?
         make-HeterogeneousVector
         HeterogeneousVector: HeterogeneousVector?
         Poly-names: Poly-fresh:
         PolyDots-names:
         PolyRow-names: PolyRow-fresh:
         -unsafe-intersect
         Mu-unsafe: Poly-unsafe:
         PolyDots-unsafe:
         Mu? Poly? PolyDots? PolyRow?
         Poly-n
         PolyDots-n
         Class? Row? Row:
         free-vars*
         Name/simple: Name/struct:
         unfold
         Union?
         Union-elems
         Union-fmap
         Un
         resolvable?
         Union-all:
         Union-all-flat:
         Union/set:
         Intersection?
         -refine
         Refine:
         Refine-obj:
         save-term-var-names!
         instantiate-type
         abstract-type
         instantiate-obj
         abstract-obj
         substitute-names
         DepFun/ids:
         (rename-out [Union:* Union:]
                     [Intersection:* Intersection:]
                     [make-Intersection* make-Intersection]
                     [Class:* Class:]
                     [Class* make-Class]
                     [Row* make-Row]
                     [Mu:* Mu:]
                     [Poly:* Poly:]
                     [PolyDots:* PolyDots:]
                     [PolyRow:* PolyRow:]
                     [Mu* make-Mu]
                     [make-Mu unsafe-make-Mu]
                     [Poly* make-Poly]
                     [PolyDots* make-PolyDots]
                     [PolyRow* make-PolyRow]
                     [Mu-body* Mu-body]
                     [Mu-body Mu-body-unsafe]
                     [Poly-body* Poly-body]
                     [PolyDots-body* PolyDots-body]
                     [PolyRow-body* PolyRow-body]
                     [Intersection-prop* Intersection-prop]))

(define (resolvable? x)
  (or (Mu? x)
      (Name? x)
      (App? x)))

(lazy-require
 ("../types/overlap.rkt" (overlap?))
 ("../types/prop-ops.rkt" (-and))
 ("../types/resolve.rkt" (resolve-app))
 ("../infer/infer.rkt" (intersect)))

;; tables that save variables from parsed types
;; so that later printing/checking can use the
;; the same variables
(define type-var-name-table (make-hash))
(define term-var-name-table (make-hash))

;; Name = Symbol

;; Type is defined in rep-utils.rkt

;; this is ONLY used when a type error ocurrs
;; FIXME: add a safety so this type can literally
;; ONLY be used when raising type errors, since
;; it's a dangerous type to have accidently floating around
;; as it is both Top and Bottom.
(def-type Error () [#:singleton Err])

;;************************************************************
;; Type Variables/Applications
;;************************************************************


;; de Bruijn indexes - should never appear outside of this file
;; bound type variables
;; i is an nat
(def-type B ([i natural-number/c]) #:base)

;; free type variables
;; n is a Name
(def-type F ([n symbol?])
  [#:frees
   [#:vars (_) (single-free-var n)]
   [#:idxs (_) empty-free-vars]]
  [#:fmap (_ #:self self) self]
  [#:for-each (_) (void)])

(define Name-table (make-free-id-table))

;; Name, an indirection of a type through the environment
;;
;; interp.
;; A type name, potentially recursive or mutually recursive or pointing
;; to a type for a struct type
;; id is the name stored in the environment
;; args is the number of arguments expected by this Name type
;; struct? indicates if this maps to a struct type
(def-type Name ([id identifier?]
                [args exact-nonnegative-integer?]
                [struct? boolean?])
  #:base
  [#:custom-constructor
   (free-id-table-ref! Name-table id (λ () (make-Name id args struct?)))])

;; rator is a type
;; rands is a list of types
(def-type App ([rator Type?]
               [rands (listof Type?)])
  [#:frees (f)
   (match rator 
     [(Name: n _ _)
      (instantiate-frees n (map f rands))]
     [_ (f (resolve-app rator rands))])]
  [#:fmap (f) (make-App (f rator) (map f rands))]
  [#:for-each (f)
   (f rator)
   (for-each f rands)])


;;************************************************************
;; Structural Types
;;************************************************************

;; structural types
;; these have only Type? fields, for which they specify their variance
;; (either #:covariant, #:contravariant, or #:invariant for Covariant, Contravariant, or Invariant)
;; instead of specifying a contract for the fields
(define-syntax (def-structural stx)
  (define-syntax-class (structural-flds frees)
    #:attributes (name variance fld-frees)
    (pattern [name:id #:covariant]
             #:with variance #'variance:co
             #:with fld-frees #'(frees name))
    (pattern [name:id #:contravariant]
             #:with variance #'variance:contra
             #:with fld-frees #'(flip-variances (frees name)))
    (pattern [name:id #:invariant]
             #:with variance #'variance:inv
             #:with fld-frees #'(make-invariant (frees name))))
  (syntax-parse stx
    [(_ name:var-name ((~var flds (structural-flds #'frees)) ...) . rst)
     (quasisyntax/loc stx
       (def-rep name ([flds.name Type?] ...)
         [#:parent Type]
         [#:frees (frees) . #,(if (= 1 (length (syntax->list #'(flds.name ...))))
                                  #'(flds.fld-frees ...)
                                  #'((combine-frees (list flds.fld-frees ...))))]
         [#:fmap (f) (name.constructor (f flds.name) ...)]
         [#:for-each (f) (f flds.name) ...]
         [#:variances (list flds.variance ...)]
         . rst))]))


;;--------
;; Pairs
;;--------

;; left and right are Types
(def-structural Pair ([left #:covariant]
                      [right #:covariant])
  [#:mask mask:pair])

;;----------------
;; Mutable Pairs
;;----------------

(def-type MPairTop ()
  [#:mask mask:mpair]
  [#:singleton -MPairTop])

;; *mutable* pairs - distinct from regular pairs
;; left and right are Types
(def-structural MPair ([left #:invariant] [right #:invariant])
  [#:mask mask:mpair])

;;----------
;; Vectors
;;----------

(def-structural Immutable-Vector ([elem #:covariant])
  [#:mask mask:immutable-vector])

(def-type Mutable-VectorTop ()
  [#:mask mask:mutable-vector]
  [#:singleton -Mutable-VectorTop])

(def-structural Mutable-Vector ([elem #:invariant])
  [#:mask mask:mutable-vector])

(define-match-expander Vector:
  (lambda (stx)
    (syntax-parse stx
     [(_ elem-pat)
      #'(or (Immutable-Vector: elem-pat)
            (Mutable-Vector: elem-pat)
            ;; The `Union-all` cases are matching an unordered list, basically:
            ;;  `(list-no-order (IV: elem-pat) (MV: other-elem-pat))
            ;;   #:when (equal? elem-pat other-elem-pat)`
            ;; but using an `or` instead of an equality constraint. See also:
            ;;  <https://github.com/racket/racket/issues/1304>
            (Union-all: (list (Immutable-Vector: elem-pat)
                              (Mutable-Vector: elem-pat)))
            (Union-all: (list (Mutable-Vector: elem-pat)
                              (Immutable-Vector: elem-pat))))])))

(define Vector?
  (let ([im-vec? (lambda (x) (or (Immutable-Vector? x) (Mutable-Vector? x)))])
    (lambda (x)
      (or (im-vec? x)
          (let ([elems (Union-all-list? x)])
            (and elems (andmap im-vec? elems)))))))

;;------
;; Box
;;------

(def-type BoxTop ()
  [#:mask mask:box]
  [#:singleton -BoxTop])

(def-structural Box ([elem #:invariant])
  [#:mask mask:box])

;;----------
;; Channel
;;----------

(def-type ChannelTop ()
  [#:mask mask:channel]
  [#:singleton -ChannelTop])

(def-structural Channel ([elem #:invariant])
  [#:mask mask:channel])

;;----------------
;; Async-Channel
;;----------------

(def-type Async-ChannelTop ()
  [#:mask mask:channel]
  [#:singleton -Async-ChannelTop])

(def-structural Async-Channel ([elem #:invariant])
  [#:mask mask:channel])

;;-------------
;; ThreadCell
;;-------------

(def-type ThreadCellTop ()
  [#:mask mask:thread-cell]
  [#:singleton -ThreadCellTop])

(def-structural ThreadCell ([elem #:invariant])
  [#:mask mask:thread-cell])

;;----------
;; Promise
;;----------

(def-structural Promise ([elem #:covariant])
  [#:mask mask:promise])

;;------------
;; Ephemeron
;;------------

(def-structural Ephemeron ([elem #:covariant])
  [#:mask mask:ephemeron])


;;-----------
;; Weak-Box
;;-----------

(def-type Weak-BoxTop ()
  [#:mask mask:other-box]
  [#:singleton -Weak-BoxTop])

(def-structural Weak-Box ([elem #:invariant])
  [#:mask mask:other-box])


;;---------------
;; CustodianBox
;;---------------

(def-structural CustodianBox ([elem #:covariant])
  [#:mask mask:other-box])

;;------
;; Set
;;------

;; TODO separate mutable/immutable set types
(def-structural Set ([elem #:covariant])
  [#:mask mask:set])

;;------------
;; HashTable
;;------------

(def-structural Immutable-HashTable ([key #:covariant] [value #:covariant])
  [#:mask mask:immutable-hash])

(def-type Mutable-HashTableTop ()
  [#:mask mask:mutable-hash]
  [#:singleton -Mutable-HashTableTop])

(def-structural Mutable-HashTable ([key #:invariant] [value #:invariant])
  [#:mask mask:mutable-hash])

(def-type Weak-HashTableTop ()
  [#:mask mask:weak-hash]
  [#:singleton -Weak-HashTableTop])

(def-structural Weak-HashTable ([key #:invariant] [value #:invariant])
  [#:mask mask:weak-hash])


;;------
;; Evt
;;------

(def-structural Evt ([result #:covariant]))

;;--------
;; Param
;;--------

(def-structural Param ([in #:contravariant]
                       [out #:covariant])
  [#:mask mask:procedure])


;;---------
;; Syntax
;;---------

;; t is the type of the result of syntax-e, not the result of syntax->datum
(def-structural Syntax ([t #:covariant])
  [#:mask mask:syntax])

;;---------
;; Future
;;---------

(def-structural Future ([t #:covariant])
  [#:mask mask:future])


;;---------------
;; Prompt-Tagof
;;---------------

(def-type Prompt-TagTop ()
  [#:mask mask:prompt-tag]
  [#:singleton -Prompt-TagTop])

;; body: the type of the body
;; handler: the type of the prompt handler
;;   prompts with this tag will return a union of `body` 
;;   and the codomains of `handler`
(def-structural Prompt-Tagof ([body #:invariant]
                              [handler #:invariant])
  [#:mask mask:prompt-tag])

;;--------------------------
;; Continuation-Mark-Keyof
;;--------------------------

(def-type Continuation-Mark-KeyTop ()
  [#:mask mask:continuation-mark-key]
  [#:singleton -Continuation-Mark-KeyTop])

;; value: the type of allowable values
(def-structural Continuation-Mark-Keyof ([value #:invariant])
  [#:mask mask:continuation-mark-key])

;;************************************************************
;; List/Vector Types (that are not simple structural types)
;;************************************************************

;; dotted list -- after expansion, becomes normal Pair-based list type
(def-type ListDots ([dty Type?] [dbound (or/c symbol? natural-number/c)])
  [#:frees
   [#:vars (f)
    (if (symbol? dbound)
        (free-vars-remove (f dty) dbound)
        (f dty))]
   [#:idxs (f)
    (if (symbol? dbound)
        (combine-frees (list (single-free-var dbound) (f dty)))
        (f dty))]]
  [#:fmap (f) (make-ListDots (f dty) dbound)]
  [#:for-each (f) (f dty)])


;; elems are all Types
(def-type Immutable-HeterogeneousVector ([elems (listof Type?)])
  [#:frees (f) (combine-frees (map f elems))]
  [#:fmap (f) (make-Immutable-HeterogeneousVector (map f elems))]
  [#:for-each (f) (for-each f elems)]
  [#:mask mask:immutable-vector])

(def-type Mutable-HeterogeneousVector ([elems (listof Type?)])
  [#:frees (f) (make-invariant (combine-frees (map f elems)))]
  [#:fmap (f) (make-Mutable-HeterogeneousVector (map f elems))]
  [#:for-each (f) (for-each f elems)]
  [#:mask mask:mutable-vector])

(define (make-HeterogeneousVector ts)
  (Un (make-Immutable-HeterogeneousVector ts)
      (make-Mutable-HeterogeneousVector ts)))

(define-match-expander HeterogeneousVector:
  (lambda (stx)
    (syntax-parse stx
     [(_ elem-pats)
      #'(or (Immutable-HeterogeneousVector: elem-pats)
            (Mutable-HeterogeneousVector: elem-pats)
            ;; See comment above about `list-no-order`
            (Union-all: (list (Immutable-HeterogeneousVector: elem-pats)
                              (Mutable-HeterogeneousVector: elem-pats)))
            (Union-all: (list (Mutable-HeterogeneousVector: elem-pats)
                              (Immutable-HeterogeneousVector: elem-pats))))])))
(define HeterogeneousVector?
  (let ([im-hvec? (lambda (x) (or (Immutable-HeterogeneousVector? x)
                                  (Mutable-HeterogeneousVector? x)))])
    (lambda (x)
      (or (im-hvec? x)
          (let ([elems (Union-all-list? x)])
            (and elems (andmap im-hvec? elems)))))))



;;************************************************************
;; Type Binders (Polys, Mus, etc)
;;************************************************************



(def-type Mu ([body Type?])
  #:no-provide
  [#:frees (f) (f body)]
  [#:fmap (f) (make-Mu (f body))]
  [#:for-each (f) (f body)]
  [#:mask (λ (t) (mask (Mu-body t)))]
  [#:custom-constructor
   (cond
     [(Bottom? body) -Bottom]
     [(or (Base? body)
          (BaseUnion? body))
      body]
     [else (make-Mu body)])])

;; n is how many variables are bound here
;; body is a type
(def-type Poly ([n exact-nonnegative-integer?]
                [body Type?])
  #:no-provide
  [#:frees (f) (f body)]
  [#:fmap (f) (make-Poly n (f body))]
  [#:for-each (f) (f body)]
  [#:mask (λ (t) (mask (Poly-body t)))])

;; n is how many variables are bound here
;; there are n-1 'normal' vars and 1 ... var
(def-type PolyDots ([n exact-nonnegative-integer?]
                    [body Type?])
  #:no-provide
  [#:frees (f) (f body)]
  [#:fmap (f) (make-PolyDots n (f body))]
  [#:for-each (f) (f body)]
  [#:mask (λ (t) (mask (PolyDots-body t)))])

;; interp. A row polymorphic function type
;; constraints are row absence constraints, represented
;; as a set for each of init, field, methods
(def-type PolyRow ([constraints (list/c list? list? list? list?)]
                   [body Type?])
  #:no-provide
  [#:frees (f) (f body)]
  [#:fmap (f) (make-PolyRow constraints (f body))]
  [#:for-each (f) (f body)]
  [#:mask (λ (t) (mask (PolyRow-body t)))])


(def-type Opaque ([pred identifier?])
  #:base
  [#:custom-constructor (make-Opaque (normalize-id pred))])



;;************************************************************
;; Functions, Arrows
;;************************************************************


;; keyword arguments
(def-rep Keyword ([kw keyword?] [ty Type?] [required? boolean?])
  [#:frees (f) (f ty)]
  [#:fmap (f) (make-Keyword kw (f ty) required?)]
  [#:for-each (f) (f ty)])

(define/provide (Keyword<? kw1 kw2)
  (keyword<? (Keyword-kw kw1)
             (Keyword-kw kw2)))

;; contract for a sorted keyword list
(define-for-cond-contract (keyword-sorted/c kws)
  (or (null? kws)
      (= (length kws) 1)
      (equal? kws (sort kws Keyword<?))))

;; a Rest argument description
;; tys: the cycle describing the rest args
;; e.g.
;; tys = (list Number) means all provided rest args
;;        must all be a Number (see `+')
;; tys = (list A B) means the rest arguments must be
;;       of even cardinality, and must be an A followed
;;       by a B repeated (e.g. A B A B A B)
;; etc
(def-rep Rest ([tys (cons/c Type? (listof Type?))])
  [#:frees (f) (combine-frees (map f tys))]
  [#:fmap (f) (make-Rest (map f tys))]
  [#:for-each (f) (for-each f tys)])

(def-rep RestDots ([ty Type?]
                   [nm (or/c natural-number/c symbol?)])
  [#:frees
   [#:vars (f)
    (cond
      [(symbol? nm) (free-vars-remove (f ty) nm)]
      [else (f ty)])]
   [#:idxs (f)
    (cond
      [(symbol? nm) (combine-frees (list (f ty) (single-free-var nm)))]
      [else (f ty)])]]
  [#:fmap (f) (make-RestDots (f ty) nm)]
  [#:for-each (f) (f ty)])


(def-rep Arrow ([dom (listof Type?)]
                [rst (or/c #f Rest? RestDots?)]
                [kws (and/c (listof Keyword?) keyword-sorted/c)]
                [rng SomeValues?])
  [#:frees (f)
   (combine-frees
    (list* (f rng)
           (if rst
               (flip-variances (f rst))
               empty-free-vars)
           (append
            (for/list ([kw (in-list kws)])
              (flip-variances (f kw)))
            (for/list ([d (in-list dom)])
              (flip-variances (f d))))))]
  [#:fmap (f) (make-Arrow (map f dom)
                          (and rst (f rst))
                          (map f kws)
                          (f rng))]
  [#:for-each (f)
   (for-each f dom)
   (when rst (f rst))
   (for-each f kws)
   (f rng)])

(define/provide (Arrow-min-arity a)
  (length (Arrow-dom a)))

(define/provide (Arrow-max-arity a)
  (if (Type? (Arrow-rst a))
      +inf.0
      (length (Arrow-dom a))))

(define/provide Arrow-includes-arity?
  (case-lambda
    [(arrow arity) (Arrow-includes-arity? (Arrow-dom arrow)
                                          (Arrow-rst arrow)
                                          arity)]
    [(dom rst raw-arity)
     (define dom-len (length dom))
     (define arity (if (number? raw-arity)
                       raw-arity
                       (length raw-arity)))
     (cond
       [(< arity dom-len) #f]
       [(= arity dom-len) #t]
       [else
        (match rst
          [(Rest: (app length rst-len))
           (define extra-args (- arity dom-len))
           (zero? (remainder extra-args rst-len))]
          [_ #f])])]))

(define/provide (Arrow-domain-at-arity a arity)
  (define dom-len (length (Arrow-dom a)))
  (cond
    [(> dom-len arity)
     (error 'Arrow-domain-at-arity
            "invalid arity! ~a @ ~a" a arity)]
    [(= arity dom-len) (Arrow-dom a)]
    [(Arrow-rst a)
     => (match-lambda
          [(Rest: rst-ts)
           (define extra-args (- arity dom-len))
           (define-values (reps extra)
             (quotient/remainder extra-args (length rst-ts)))
           (unless (zero? extra)
             (error 'Arrow-domain-at-arity
                    "invalid arity! ~a @ ~a" a arity))
           (append (Arrow-dom a) (repeat-list rst-ts reps))]
          [_ #f])]
    [else
     (error 'Arrow-domain-at-arity
            "invalid arity! ~a @ ~a" a arity)]))

;; a standard function
;; + all functions are case-> under the hood (i.e. see 'arrows')
;; + each Arrow in 'arrows' may have a dependent range
(def-type Fun ([arrows (listof Arrow?)])
  [#:mask mask:procedure]
  [#:frees (f) (combine-frees (map f arrows))]
  [#:fmap (f) (make-Fun (map f arrows))]
  [#:for-each (f) (for-each f arrows)])


;; a function with dependent arguments and/or a pre-condition
(def-type DepFun ([dom (listof Type?)]
                  [pre Prop?]
                  [rng SomeValues?])
  [#:mask mask:procedure]
  [#:frees (f) (combine-frees (list* (f rng)
                                     (flip-variances (f pre))
                                     (for/list ([d (in-list dom)])
                                       (flip-variances (f d)))))]
  [#:fmap (f) (make-DepFun (map f dom) (f pre) (f rng))]
  [#:for-each (f) (for-each f dom) (f pre) (f rng)])


(define-match-expander DepFun/ids:
  (λ (stx)
    (syntax-case stx ()
      [(_ ids dom pre rng)
       (quasisyntax/loc stx
         (app (match-lambda
                [(DepFun: raw-dom raw-pre raw-rng)
                 (define fresh-ids (for/list ([_ (in-list raw-dom)]) (genid)))
                 (define (instantiate rep) (instantiate-obj rep fresh-ids))
                 (list fresh-ids
                       (map instantiate raw-dom)
                       (instantiate raw-pre)
                       (instantiate raw-rng))]
                [_ #f])
              (list ids dom pre rng)))])))


;;************************************************************
;; Structs
;;************************************************************



(def-rep fld ([t Type?] [acc identifier?] [mutable? boolean?])
  [#:frees (f) (if mutable? (make-invariant (f t)) (f t))]
  [#:fmap (f) (make-fld (f t) acc mutable?)]
  [#:for-each (f) (f t)]
  [#:custom-constructor (make-fld t (normalize-id acc) mutable?)])

;; poly? : is this type polymorphically variant
;;         If not, then the predicate is enough for higher order checks
;; pred-id : identifier for the predicate of the struct
(def-type Struct ([name identifier?]
                  [parent (or/c #f Struct?)]
                  [flds (listof fld?)]
                  [proc (or/c #f Fun?)]
                  [poly? boolean?]
                  [pred-id identifier?])
  [#:frees (f) (combine-frees (map f (append (if proc (list proc) null)
                                             (if parent (list parent) null)
                                             flds)))]
  [#:fmap (f) (make-Struct name
                           (and parent (f parent))
                           (map f flds)
                           (and proc (f proc))
                           poly?
                           pred-id)]
  [#:for-each (f)
   (when parent (f parent))
   (for-each f flds)
   (when proc (f proc))]
  ;; This should eventually be based on understanding of struct properties.
  [#:mask (mask-union mask:struct mask:procedure)]
  [#:custom-constructor
   (let ([name (normalize-id name)]
         [pred-id (normalize-id pred-id)])
     (make-Struct name parent flds proc poly? pred-id))])


(def-type StructTop ([name Struct?])
  [#:frees (f) (f name)]
  [#:fmap (f) (make-StructTop (f name))]
  [#:for-each (f) (f name)]
  [#:mask (mask-union mask:struct mask:procedure)])

;; Represents prefab structs
;; key  : prefab key encoding mutability, auto-fields, etc.
;; flds : the types of all of the prefab fields
(def-type Prefab ([key prefab-key?]
                  [flds (listof Type?)])
  [#:frees (f) (combine-frees (map f flds))]
  [#:fmap (f) (make-Prefab key (map f flds))]
  [#:for-each (f) (for-each f flds)]
  [#:mask mask:prefab])

(def-type PrefabTop ([key prefab-key?])
  #:base
  [#:mask mask:prefab]
  [#:custom-constructor
   (cond
     [(prefab-key/mutable-fields? key)
      (make-PrefabTop key)]
     [else
      (make-Prefab key (build-list (prefab-key->field-count key)
                                   (λ (_) Univ)))])])

(def-type StructTypeTop ()
  [#:mask mask:struct-type]
  [#:singleton -StructTypeTop])

;; A structure type descriptor
(def-type StructType ([s (or/c F? B? Struct? Prefab?)])
  [#:frees (f) (f s)]
  [#:fmap (f) (make-StructType (f s))]
  [#:for-each (f) (f s)]
  [#:mask mask:struct-type])


;;************************************************************
;; Singleton Values (see also Base)
;;************************************************************


;; v : Racket Value
;; contract will change to the following after
;; base types are redone:
(def-type Value ([val any/c])
  #:base
  [#:mask (λ (t) (match (Value-val t)
                   [(? number?) mask:number]
                   [(? symbol?) mask:base]
                   [(? string?) mask:base]
                   [(? char?) mask:base]
                   [_ mask:unknown]))]
  [#:custom-constructor
   (match val
     [#f -False]
     [#t -True]
     ['() -Null]
     [(? void?) -Void]
     [0 -Zero]
     [1 -One]
     [(? (lambda (x) (eq? x unsafe-undefined))) -Unsafe-Undefined]
     [_ (make-Value val)])])


;;************************************************************
;; Unions
;;************************************************************


;; mask - cached type mask
;; base - any Base types, or Bottom if none are present
;; ts - the list of types in the union (contains no duplicates,
;; gives us deterministic iteration order)
;; elems - the set equivalent of 'ts', useful for equality
;; and constant time membership tests
;; NOTE: The types contained in a union have had complicated
;; invariants in the past. Currently, we are using a few simple
;; guidelines:
;; 1. Unions do not contain duplicate types
;; 2. Unions do not contain Univ or Bottom
;; 3. Unions do not contain 'Base' or 'BaseUnion'
;;    types outside of the 'base' field.
;; That's it -- we may contain some redundant types,
;; but in general its quicker to not worry about those
;; until we're printing to the user or generating contracts,
;; at which point the 'normalize-type' function from 'types/union.rkt'
;; is used to remove overlapping types from unions.
(def-type Union ([mask type-mask?]
                 [base (or/c Bottom? Base? BaseUnion?)]
                 [ts (cons/c Type? (listof Type?))]
                 [elems (hash/c Type? #t #:immutable #t #:flat? #t)])
  #:no-provide
  #:non-transparent
  [#:frees (f) (combine-frees (map f ts))]
  [#:fmap (f) (Union-fmap f base ts)]
  [#:for-each (f) (for-each f ts)]
  [#:mask (λ (t) (Union-mask t))]
  [#:custom-constructor/contract
   (-> type-mask?
       (or/c Bottom? Base? BaseUnion?)
       (listof Type?)
       (hash/c Type? #t #:immutable #t #:flat? #t)
       Type?)
   ;; make sure we do not build Unions equivalent to
   ;; Bottom, a single BaseUnion, or a single type
   (cond
     [(hash-has-key? elems Univ) Univ]
     [else
      (match (hash-count elems)
        [0 base]
        [1 #:when (Bottom? base) (hash-iterate-key elems (hash-iterate-first elems))]
        [_ (intern-double-ref!
            union-intern-table
            elems
            base
            ;; now, if we need to build a new union, remove duplicates from 'ts'
            #:construct (make-Union mask base (remove-duplicates ts) elems))])])])

(define union-intern-table (make-weak-hash))

;; Custom match expanders for Union that expose various
;; components or combinations of components
(define-match-expander Union:*
  (syntax-rules () [(_ b ts) (Union: _ b ts _)]))

(define-match-expander Union/set:
  (syntax-rules () [(_ b ts elems) (Union: _ b ts elems)]))

(define-match-expander Union-all:
  (syntax-rules () [(_ elems) (app Union-all-list? (? list? elems))]))

(define-match-expander Union-all-flat:
  (syntax-rules () [(_ elems) (app Union-all-flat-list? (? list? elems))]))

;; returns all of the elements of a Union (sans Bottom),
;; and any BaseUnion is left in tact
;; if a non-Union is passed, returns #f
(define (Union-all-list? t)
  (match t
    [(Union: _ (? Bottom? b) ts _) ts]
    [(Union: _ b ts _) (cons b ts)]
    [_ #f]))

;; returns all of the elements of a Union (sans Bottom),
;; and any BaseUnion is flattened into the atomic Base elements
;; if a non-Union is passed, returns #f
(define (Union-all-flat-list? t)
  (match t
    [(Union: _ b ts _)
     (match b
       [(? Bottom?) ts]
       [(BaseUnion-bases: bs) (append bs ts)]
       [_ (cons b ts)])]
    [_ #f]))

;; Union-fmap
;;
;; maps function 'f' over 'base-arg' and 'args', producing a Union
;; of all of the arguments.
;;
;; This is often used in functions that walk over and rebuild types
;; in the following form:
;; (match t
;;  [(Union: b ts) (Union-fmap f b ts)]
;;  ...)
;;
;; Note: this is also the core constructor for all Unions!
(define/cond-contract (Union-fmap f base-arg args)
  (-> procedure? (or/c Bottom? Base? BaseUnion?) (listof Type?) Type?)
  ;; these fields are destructively updated during this process
  (define m mask:bottom)
  (define bbits #b0)
  (define nbits #b0)
  (define ts '())
  (define elems (hash))
  ;; add a Base element to the union
  (define (add-base! numeric? bits)
    (cond
      [numeric? (set! nbits (nbits-union nbits bits))]
      [else (set! bbits (bbits-union bbits bits))]))
  ;; add a BaseUnion to the union
  (define (add-base-union! bbits* nbits*)
    (set! nbits (nbits-union nbits nbits*))
    (set! bbits (bbits-union bbits bbits*)))
  ;; add the type from a 'base' field of a Union to this union
  (define (add-any-base! b)
    (match b
      [(? Bottom?) (void)]
      [(Base-bits: numeric? bits) (add-base! numeric? bits)]
      [(BaseUnion: bbits* nbits*) (add-base-union! bbits* nbits*)]))
  ;; apply 'f' to a type and add it to the union appropriately
  (define (process! arg)
    (match (f arg)
      [(? Bottom?) (void)]
      [(Base-bits: numeric? bits) (add-base! numeric? bits)]
      [(BaseUnion: bbits* nbits*) (add-base-union! bbits* nbits*)]
      [(Union: m* b* ts* _)
       (set! m (mask-union m m*))
       (add-any-base! b*)
       (set! ts (append ts* ts))
       (for ([t* (in-list ts*)])
         (set! elems (hash-set elems t* #t)))]
      [t (set! m (mask-union m (mask t)))
         (set! ts (cons t ts))
         (set! elems (hash-set elems t #t))]))
  ;; process the input arguments
  (process! base-arg)
  (for-each process! args)
  ;; construct a BaseUnion (or Base or Bottom) based on the
  ;; Base data gathered during processing
  (define bs (make-BaseUnion bbits nbits))
  ;; call the Union smart constructor
  (make-Union (mask-union m (mask bs))
              bs
              ts
              elems))

(define (Un . args)
  (Union-fmap (λ (x) x) -Bottom args))

;;************************************************************
;; Intersection/Refinement
;;************************************************************


;; Intersection
;; ts - the list of types (gives deterministic behavior)
;; elems - the set equivalent of 'ts', useful for equality tests
(def-type Intersection ([ts (cons/c Type? (listof Type?))]
                        [prop (and/c Prop? (not/c FalseProp?))]
                        [elems (hash/c Type? #t #:immutable #t #:flat? #t)])
  #:non-transparent
  #:no-provide
  [#:frees (f) (combine-frees (cons (f prop) (map f ts)))]
  [#:fmap (f) (-refine
               (for*/fold ([res Univ])
                          ([t (in-list ts)]
                           [t (in-value (f t))])
                 (intersect res t))
               (f prop))]
  [#:for-each (f) (for-each f ts) (f prop)]
  [#:mask (λ (t) (for/fold ([m mask:unknown])
                           ([elem (in-list (Intersection-ts t))])
                   (mask-intersect m (mask elem))))]
  [#:custom-constructor
   (intern-double-ref!
    intersection-table
    elems
    prop
    #:construct (make-Intersection (remove-duplicates ts) prop elems))])

(define intersection-table (make-weak-hash))

(define (make-Intersection* ts)
  (apply -unsafe-intersect ts))

;;  constructor for intersections
;; in general, intersections should be built
;; using the 'intersect' operator, which worries
;; about actual subtyping, etc...
(define -unsafe-intersect
  (case-lambda
    [() Univ]
    [(t) t]
    [args
     (let loop ([ts '()]
                [elems (hash)]
                [prop -tt]
                [args args])
       (match args
         [(list)
          (match ts
            [(list) (-refine Univ prop)]
            [(list t) (-refine t prop)]
            [_ (let ([t (make-Intersection ts -tt elems)])
                 (-refine t prop))])]
         [(cons arg args)
          (match arg
            [(Univ:) (loop ts elems prop args)]
            [(Intersection: ts* (TrueProp:) _) (loop ts elems prop (append ts* args))]
            [(Intersection: ts* prop* _)
             (loop ts
                   elems
                   (-and prop* prop)
                   (append ts* args))]
            [_ #:when (for/or ([elem (in-list args)])
                        (not (overlap? elem arg)))
               -Bottom]
            [t (loop (cons t ts) (hash-set elems t #t) prop args)])]))]))

(define/provide (Intersection-w/o-prop t)
  (match t
    [(Intersection: _ (TrueProp:) _) t]
    [(Intersection: (list t) prop _) t]
    [(Intersection: ts prop tset) (make-Intersection ts -tt tset)]))

;; -refine
;;
;; Constructor for refinements
;;
;; (-refine t p) constructs a refinement type
;; and assumes 'p' is already in the locally nameless form
;;
;; (-refine x t p) constructs a refinement type
;; after abstracting the identifier 'x' from 'p'
(define/provide -refine
  (case-lambda
    [(t prop)
     (match* (t prop)
       [(_ (TrueProp:)) t]
       [(_ (FalseProp:)) -Bottom]
       [(_ (TypeProp: (Path: '() (cons 0 0)) t*)) (intersect t t*)]
       [((Intersection: ts (TrueProp:) tset) _) (make-Intersection ts prop tset)]
       [((Intersection: ts prop* tset) _)
        (-refine (make-Intersection ts -tt tset) (-and prop prop*))]
       [(_ _) (make-Intersection (list t) prop (hash t #t))])]
    [(nm t prop) (-refine t (abstract-obj prop nm))]))

(define-match-expander Intersection:*
  (λ (stx) (syntax-case stx ()
             [(_ ts prop) (syntax/loc stx (Intersection ts prop _))]
             [(_ x ts prop) (syntax/loc stx (and (Intersection ts _ _)
                                                 (app (λ (i)
                                                        (define x (genid))
                                                        (cons x (Intersection-prop* (-id-path x) i)))
                                                      (cons x prop))))])))

(define-match-expander Refine:
  (λ (stx) (syntax-case stx ()
             [(_ t prop)
              (syntax/loc stx
                (and (Intersection: _ (and (not (TrueProp:)) prop) _)
                     (app Intersection-w/o-prop t)))]
             [(_ x t prop)
              (syntax/loc stx
                (and (Intersection _ (not (TrueProp:)) _)
                     (app Intersection-w/o-prop t)
                     (app (λ (i)
                            (define x (genid))
                            (cons x (Intersection-prop* (-id-path x) i)))
                          (cons x prop))))])))

(define (save-term-var-names! t xs)
  (hash-set! term-var-name-table t
             (map (λ (id) (symbol->fresh-pretty-normal-id (syntax->datum id))) xs)))

(define-match-expander Refine-obj:
  (λ (stx) (syntax-case stx ()
             [(_ obj t prop)
              (syntax/loc stx
                (and (Intersection _ (not (TrueProp:)) _)
                     (app Intersection-w/o-prop t)
                     (app (λ (i) (Intersection-prop* obj i)) prop)))])))


(define (Intersection-prop* obj t)
  (define p (Intersection-prop t))
  (and p (instantiate-obj p obj)))



;; refinement based on some predicate function 'pred'
(def-type Refinement ([parent Type?] [pred identifier?])
  [#:frees (f) (f parent)]
  [#:fmap (f) (make-Refinement (f parent) pred)]
  [#:for-each (f) (f parent)]
  [#:mask (λ (t) (mask (Refinement-parent t)))]
  [#:custom-constructor (make-Refinement parent (normalize-id pred))])


;;************************************************************
;; Object Oriented
;;************************************************************



;; A Row used in type instantiation
;; For now, this should not appear in user code. It's used
;; internally to perform row instantiations and to represent
;; class types.
;;
;; invariant: all clauses are sorted by the key name
(def-rep Row ([inits (listof (list/c symbol? Type? boolean?))]
              [fields (listof (list/c symbol? Type?))]
              [methods (listof (list/c symbol? Type?))]
              [augments (listof (list/c symbol? Type?))]
              [init-rest (or/c Type? #f)])
  #:no-provide
  [#:frees (f)
   (let ([extract-frees (λ (l) (f (second l)))])
     (combine-frees
      (append (map extract-frees inits)
              (map extract-frees fields)
              (map extract-frees methods)
              (map extract-frees augments)
              (if init-rest (list (f init-rest)) null))))]
  [#:fmap (f)
   (let ([update (λ (l) (list-update l 1 f))])
     (make-Row (map update inits)
               (map update fields)
               (map update methods)
               (map update augments)
               (and init-rest (f init-rest))))]
  [#:for-each (f)
   (let ([walk (λ (l) (f (second l)))])
     (for-each walk inits)
     (for-each walk fields)
     (for-each walk methods)
     (for-each walk augments)
     (when init-rest (f init-rest)))])

(def-type ClassTop ()
  [#:mask mask:class]
  [#:singleton -ClassTop])

;; row-ext : Option<(U F B Row)>
;; row     : Row
;;
;; interp. The first field represents a row extension
;;         The second field represents the concrete row
;;         that the class starts with
;;
(def-type Class ([row-ext (or/c #f F? B? Row?)]
                 [row Row?])
  #:no-provide
  [#:frees (f)
   (combine-frees
    (append (if row-ext (list (f row-ext)) null)
            (list (f row))))]
  [#:fmap (f) (make-Class (and row-ext (f row-ext))
                          (f row))]
  [#:for-each (f)
   (when row-ext (f row-ext))
   (f row)]
  [#:mask mask:class])


;;--------------------------
;; Instance (of a class)
;;--------------------------


;; not structural because it has special subtyping,
; not just simple structural subtyping
(def-type Instance ([cls Type?])
  [#:frees (f) (f cls)]
  [#:fmap (f) (make-Instance (f cls))]
  [#:for-each (f) (f cls)]
  [#:mask mask:instance])

;;************************************************************
;; Units
;;************************************************************


;; interp:
;; name is the id of the signature
;; extends is the extended signature or #f
;; mapping maps variables in a signature to their types
;; This is not a type because signatures do not correspond to any values
(def-rep Signature ([name identifier?]
                    [extends (or/c identifier? #f)]
                    [mapping (listof (cons/c identifier? Type?))])
  [#:frees (f) (combine-frees (map (match-lambda
                                     [(cons _ t) (f t)])
                                   mapping))]
  [#:fmap (f) (make-Signature name extends (map (match-lambda
                                                  [(cons id t) (cons id (f t))])
                                                mapping))]
  [#:for-each (f) (for-each (match-lambda
                              [(cons _ t) (f t)])
                            mapping)]
  [#:custom-constructor
   (make-Signature (normalize-id name)
                   (and extends (normalize-id extends))
                   (for*/list ([p (in-list mapping)]
                               [(id ty) (in-pair p)])
                     (cons (normalize-id id) ty)))])


(def-type UnitTop ()
  [#:mask mask:unit]
  [#:singleton -UnitTop])


;; interp: imports is the list of imported signatures
;;         exports is the list of exported signatures
;;         init-depends is the list of init-depend signatures
;;         result is the type of the body of the unit
(def-type Unit ([imports (listof Signature?)]
                [exports (listof Signature?)]
                [init-depends (listof Signature?)]
                [result SomeValues?])
  [#:frees (f) (f result)]
  [#:fmap (f) (make-Unit (map f imports)
                         (map f exports)
                         (map f init-depends)
                         (f result))]
  [#:for-each (f)
   (for-each f imports)
   (for-each f exports)
   (for-each f init-depends)
   (f result)]
  [#:mask mask:unit])


;;************************************************************
;; Sequences
;;************************************************************

(def-type SequenceTop ()
  [#:singleton -SequenceTop])

;; includes lists, vectors, etc
;; tys : sequence produces this set of values at each step
(def-type Sequence ([tys (listof Type?)])
  [#:frees (f) (combine-frees (map f tys))]
  [#:fmap (f) (make-Sequence (map f tys))]
  [#:for-each (f) (for-each f tys)])

;; Distinction
;; comes from define-new-subtype
;; nm: a symbol representing the name of the type
;; id: a symbol created with gensym
;; ty: a type for the representation (i.e. each distinction
;;     is a subtype of its ty)
(def-type Distinction ([nm symbol?] [id symbol?] [ty Type?])
  [#:frees (f) (f ty)]
  [#:fmap (f) (make-Distinction nm id (f ty))]
  [#:for-each (f) (f ty)]
  [#:mask (λ (t) (mask (Distinction-ty t)))]
  [#:custom-constructor
   (if (Bottom? ty)
       -Bottom
       (make-Distinction nm id ty))])


;;************************************************************
;; Type Variable tools (i.e. Abstraction/Instantiation)
;; Note: see the 'Locally Nameless' binder
;;       representation strategy for general
;;       details on the approach we're using
;;************************************************************

;; abstract-many/type
;;
;; abstracts the type variable names from 'names-to-abstract'
;; to de bruijn indices in 'initial'.
;; Specifically, if n = (length names-to-abstract) then
;; names-to-abstract[0] gets mapped to n-1
;; names-to-abstract[1] gets mapped to n-2
;; ...
;; names-to-abstract[n-1] gets mapped to 0
(define/cond-contract (abstract-type initial names-to-abstract)
  (-> Rep? (or/c symbol? (listof symbol?)) Rep?)
  (cond
    [(null? names-to-abstract) initial]
    [(not (pair? names-to-abstract))
     (abstract-type initial (list names-to-abstract))]
    [else
     (define n-1 (sub1 (length names-to-abstract)))
     (define (abstract-name name lvl default dotted?)
       (cond
         [(symbol? name)
          (match (index-of names-to-abstract name eq?)
            [#f default]
            ;; adjust index properly before using (see comments above
            ;; and note we are under 'lvl' additional binders)
            [idx (let ([idx (+ lvl (- n-1 idx))])
                   (cond [dotted? idx]
                         [else (make-B idx)]))])]
         [else default]))
     (type-var-transform initial abstract-name)]))


;; instantiate-type
;;
;; instantiates type De Bruijn indices 0 (i.e. (B 0))
;; through (sub1 (length images)) with images.
;; (i.e. De Bruin i = (B i))
;; Specifically, if n = (length images), then
;; index 0 gets mapped to images[n-1]
;; index 1 gets mapped to images[n-2]
;; ...
;; index n-1 gets mapped to images[0]
(define/cond-contract (instantiate-type initial images)
  (-> Rep? (or/c Type? (listof Type?)) Rep?)
  (cond
    [(null? images) initial]
    [(not (pair? images)) (instantiate-type initial (list images))]
    [else
     (define n-1 (sub1 (length images)))
     (define (instantiate-idx idx lvl default dotted?)
       (cond
         [(exact-nonnegative-integer? idx)
          ;; adjust for being under 'depth' binders and for
          ;; index 0 gets mapped to images[n-1], etc
          (let ([idx (- n-1 (- idx lvl))])
            (match (list-ref/default images idx #f)
              [#f default]
              [image (cond [dotted? (F-n image)]
                           [else image])]))]
         [else default]))
     (type-var-transform initial instantiate-idx)]))



;; type-var-transform
;;
;; Helper function for instantiate[-many]/type
;; and abstract[-many]/type.
;;
;; transform : [target : (or nat sym)]
;;             [lvl : nat]
;;             [default : (or Type nat sym)]
;;             [dotted? : boolean]
;;             ->
;;             (or Type nat sym)
;; where 'target' is the thing potentially being replaced
;; 'depth' is how many binders we're under
;; 'default' is what it uses if we're not replacing 'target'
;; 'dotted?' is a flag denoting if this is a dotted var/idx
(define (type-var-transform initial transform)
  (let rec/lvl ([cur initial] [lvl 0])
    (define (rec rep) (rec/lvl rep lvl))
    (match cur
      ;; De Bruijn indices
      [(B: idx) (transform idx lvl cur #f)]
      ;; Type variables
      [(F: var) (transform var lvl cur #f)]
      ;; forms w/ dotted type vars/indices
      [(RestDots: ty d)
       (make-RestDots (rec ty) (transform d lvl d #t))]
      [(ValuesDots: rs dty d)
       (make-ValuesDots (map rec rs)
                        (rec dty)
                        (transform d lvl d #t))]
      [(ListDots: dty d)
       (make-ListDots (rec dty)
                      (transform d lvl d #t))]
      ;; forms which introduce bindings (increment lvls appropriately)
      [(Mu: body) (make-Mu (rec/lvl body (add1 lvl)))]
      [(PolyRow: constraints body)
       (make-PolyRow constraints (rec/lvl body (add1 lvl)))]
      [(PolyDots: n body)
       (make-PolyDots n (rec/lvl body (+ n lvl)))]
      [(Poly: n body)
       (make-Poly n (rec/lvl body (+ n lvl)))]
      [_ (Rep-fmap cur rec)])))



;;***************************************************************
;; Dependent Function/Refinement tools
;; Note: see the 'Locally Nameless' binder
;;       representation strategy for general
;;       details on the approach we're using
;;***************************************************************



;; instantiates term De Bruijn indices
;; '(0 . 0) ... '(0 . (sub1 (length os)))
;; in 'initial with objects from 'os'
(define/cond-contract (instantiate-obj initial os)
  (-> Rep? (or/c identifier?
                 OptObject?
                 (listof (or/c identifier? OptObject?)))
      Rep?)
  (cond
    [(null? os) initial]
    [(not (pair? os)) (instantiate-obj initial (list os))]
    [else
     (define (instantiate-idx name cur-lvl)
       (match name
         [(cons lvl idx)
          #:when (eqv? lvl cur-lvl)
          (list-ref os idx)]
         [_ name]))
     (term-var-transform initial instantiate-idx)]))


;; abstracts the n identifiers from 'ids-to-abstract'
;; in 'initial', replacing them with term De Bruijn indices
;; '(0 . 0) ... '(0 . n-1) (or their appropriate
;; successors under additional binders)

(define/cond-contract (abstract-obj initial
                                    ids-to-abstract
                                    [erase-existentials? #f])
  (->* (Rep?
        (or/c identifier? (listof identifier?)))
       (boolean?)
       Rep?)
  (cond
    [(and (null? ids-to-abstract)
          (not erase-existentials?)) initial]
    [(identifier? ids-to-abstract)
     (abstract-obj initial (list ids-to-abstract))]
    [else
     (define (abstract-id id lvl)
       (cond
         [(identifier? id)
          (match (index-of ids-to-abstract id free-identifier=?)
            [#f (cond
                  [(and erase-existentials?
                        (existential-id? id))
                   -empty-obj]
                  [else id])]
            ;; adjust index properly before using (see comments above
            ;; and note we are under 'lvl' additional binders)
            [idx (cons lvl idx)])]
         [else id]))
     (term-var-transform initial abstract-id)]))


;; term-binder-transform
;;
;; Helper function for abstract[-many]/obj
;; and instantiate[-many]/obj.
;;
;; transform : [target : (or nat sym)]
;;             [depth : nat]
;;             ->
;;             (or nat sym)
;; where 'target' is the thing potentially being replaced
;; 'depth' is how many binders we're under
(define (term-var-transform initial transform)
  (let rec/lvl ([rep initial] [lvl 0])
    (define (rec rep) (rec/lvl rep lvl))
    (define (rec/inc rep) (rec/lvl rep (add1 lvl)))
    (match rep
      ;; Functions
      ;; increment the level of the substituted object
      [(Arrow: dom rst kws rng)
       (make-Arrow (map rec dom)
                   (and rst (rec rst))
                   (map rec kws)
                   (rec/inc rng))]
      [(DepFun: dom pre rng)
       (make-DepFun (for/list ([d (in-list dom)])
                      (rec/inc d))
                    (rec/inc pre)
                    (rec/inc rng))]
      ;; Refinement types e.g. {x ∈ τ | ψ(x)}
      ;; increment the level of the substituted object
      [(Intersection: ts p _) (-refine
                               (apply -unsafe-intersect (map rec ts))
                               (rec/inc p))]
      [(Path: flds nm)
       (make-Path (map rec flds)
                  (transform nm lvl))]
      [_ (Rep-fmap rep rec)])))

;; simple substitution mapping identifiers to
;; identifiers (or objects)
;; pre: (= (length names) (length names-or-objs))
(define (substitute-names rep names names-or-objs)
  (let subst ([rep rep])
    (match rep
      [(Path: flds (? identifier? name))
       (make-Path (map subst flds)
                  (match (index-of names name free-identifier=?)
                    [#f name]
                    [idx (list-ref names-or-objs idx)]))]
      [_ (Rep-fmap rep subst)])))

;;************************************************************
;; Smart Constructors/Destructors for Type Binders
;;
;; i.e. constructors and destructors which use
;; abstract/instantiate so free variables are always
;; type variables (i.e. F) and bound variables are
;; always De Bruijn indices (i.e. B)
;;************************************************************



;; the 'smart' constructor
(define (Mu* name body)
  (let ([v (make-Mu (abstract-type body name))])
    (hash-set! type-var-name-table v name)
    v))

;; the 'smart' destructor
(define (Mu-body* name t)
  (match t
    [(Mu: body)
     (instantiate-type body (make-F name))]))

;; unfold : Mu -> Type
(define/cond-contract (unfold t)
  (Mu? . -> . Type?)
  (match t
    [(Mu-unsafe: body) (instantiate-type body t)]
    [t (error 'unfold "not a mu! ~a" t)]))


;; Poly 'smart' constructor
;;
;; Also keeps track of the original name in a table to recover names
;; for debugging or to correlate with surface syntax
;;
;; Provide #:original-names if the names that you are closing off
;; are *different* from the names you want recorded in the table.
;;
;; list<symbol> type #:original-names list<symbol> -> type
;;
(define (Poly* names body #:original-names [orig names])
  (if (null? names) body
      (let ([v (make-Poly (length names) (abstract-type body names))])
        (hash-set! type-var-name-table v orig)
        v)))

;; Poly 'smart' destructor
(define (Poly-body* names t)
  (match t
    [(Poly: n body)
     (unless (= (length names) n)
       (int-err "Wrong number of names: expected ~a got ~a" n (length names)))
     (instantiate-type body (map make-F names))]))

;; PolyDots 'smart' constructor
(define (PolyDots* names body)
  (if (null? names) body
      (let ([v (make-PolyDots (length names) (abstract-type body names))])
        (hash-set! type-var-name-table v names)
        v)))

;; PolyDots 'smart' destructor
(define (PolyDots-body* names t)
  (match t
    [(PolyDots: n body)
     (unless (= (length names) n)
       (int-err "Wrong number of names: expected ~a got ~a" n (length names)))
     (instantiate-type body (map make-F names))]))


;; PolyRow 'smart' constructor
;;
;; Note that while `names` lets you specify multiple names, it's
;; expected that row polymorphic types only bind a single name at
;; a time. This may change in the future.
;;
(define (PolyRow* names constraints body #:original-names [orig names])
  (let ([v (make-PolyRow constraints (abstract-type body names))])
    (hash-set! type-var-name-table v orig)
    v))

;; PolyRow 'smart' desctrutor
(define (PolyRow-body* names t)
  (match t
    [(PolyRow: constraints body)
     (instantiate-type body (map make-F names))]))


;;***************************************************************
;; Smart Match Expanders for Type Binders
;;
;; i.e. match expanders which use the smart destructors defined
;; above -- many of these are provided w/ rename-out so they
;; are the defacto match expanders
;;***************************************************************


(define-match-expander Mu-unsafe:
  (lambda (stx)
    (syntax-case stx ()
      [(_ bp) #'(? Mu? (app (lambda (t) (Mu-body t)) bp))])))

(define-match-expander Poly-unsafe:
  (lambda (stx)
    (syntax-case stx ()
      [(_ n bp) #'(? Poly? (app (lambda (t) (list (Poly-n t) (Poly-body t))) (list n bp)))])))

(define-match-expander PolyDots-unsafe:
  (lambda (stx)
    (syntax-case stx ()
      [(_ n bp) #'(? PolyDots? (app (lambda (t) (list (PolyDots-n t) (PolyDots-body t))) (list n bp)))])))

(define-match-expander Mu:*
  (lambda (stx)
    (syntax-case stx ()
      [(_ np bp)
       #'(? Mu?
            (app (lambda (t) (let ([sym (gensym)])
                               (list sym (Mu-body* sym t))))
                 (list np bp)))])))

(define-match-expander Mu-maybe-name:
  (lambda (stx)
    (syntax-case stx ()
      [(_ np bp)
       #'(? Mu?
            (app (lambda (t) (let ([sym (hash-ref type-var-name-table t #f)])
                               (if sym
                                   (list sym (Mu-body* sym t))
                                   (list #f #f))))
                 (list np bp)))])))

;; These match expanders correspond to opening up a type in
;; locally nameless representation. When the type is opened,
;; the nameless bound variables are replaced with free
;; variables with names.
;;
;; This match expander wraps the smart constructor
;; names are generated with gensym
(define-match-expander Poly:*
  (lambda (stx)
    (syntax-case stx ()
      [(_ nps bp)
       #'(? Poly?
            (app (lambda (t)
                   (let* ([n (Poly-n t)]
                          [syms (build-list n (lambda _ (gensym)))])
                     (list syms (Poly-body* syms t))))
                 (list nps bp)))])))

;; This match expander uses the names from the hashtable
(define-match-expander Poly-names:
  (lambda (stx)
    (syntax-case stx ()
      [(_ nps bp)
       #'(? Poly?
            (app (lambda (t)
                   (let* ([n (Poly-n t)]
                          [syms (hash-ref type-var-name-table t (lambda _ (build-list n (lambda _ (gensym)))))])
                     (list syms (Poly-body* syms t))))
                 (list nps bp)))])))

;; Helper for fresh match expanders below, creates a
;; fresh name that prints the same as the original
(define (fresh-name sym)
  (string->uninterned-symbol (symbol->string sym)))

;; This match expander creates new fresh names for exploring the body
;; of the polymorphic type. When lexical scoping of type variables is a concern, you
;; should use this form.
(define-match-expander Poly-fresh:
  (lambda (stx)
    (syntax-case stx ()
      [(_ nps freshp bp)
       #'(? Poly?
            (app (lambda (t)
                   (let* ([n (Poly-n t)]
                          [syms (hash-ref type-var-name-table t (lambda _ (build-list n (lambda _ (gensym)))))]
                          [fresh-syms (map fresh-name syms)])
                     (list syms fresh-syms (Poly-body* fresh-syms t))))
                 (list nps freshp bp)))])))

;; This match expander wraps the smart constructor
;; names are generated with gensym
(define-match-expander PolyDots:*
  (lambda (stx)
    (syntax-case stx ()
      [(_ nps bp)
       #'(? PolyDots?
            (app (lambda (t)
                   (let* ([n (PolyDots-n t)]
                          [syms (build-list n (lambda _ (gensym)))])
                     (list syms (PolyDots-body* syms t))))
                 (list nps bp)))])))

;; This match expander uses the names from the hashtable
(define-match-expander PolyDots-names:
  (lambda (stx)
    (syntax-case stx ()
      [(_ nps bp)
       #'(? PolyDots?
            (app (lambda (t)
                   (let* ([n (PolyDots-n t)]
                          [syms (hash-ref type-var-name-table t (lambda _ (build-list n (lambda _ (gensym)))))])
                     (list syms (PolyDots-body* syms t))))
                 (list nps bp)))])))

(define-match-expander PolyRow:*
  (lambda (stx)
    (syntax-case stx ()
      [(_ nps constrp bp)
       #'(? PolyRow?
            (app (lambda (t)
                   (define sym (gensym))
                   (list (list sym)
                         (PolyRow-constraints t)
                         (PolyRow-body* (list sym) t)))
                 (list nps constrp bp)))])))

(define-match-expander PolyRow-names:
  (lambda (stx)
    (syntax-case stx ()
      [(_ nps constrp bp)
       #'(? PolyRow?
            (app (lambda (t)
                   (define syms (hash-ref type-var-name-table t (λ _ (list (gensym)))))
                   (list syms
                         (PolyRow-constraints t)
                         (PolyRow-body* syms t)))
                 (list nps constrp bp)))])))

(define-match-expander PolyRow-fresh:
  (lambda (stx)
    (syntax-case stx ()
      [(_ nps freshp constrp bp)
       #'(? PolyRow?
            (app (lambda (t)
                   (define syms (hash-ref type-var-name-table t (λ _ (list (gensym)))))
                   (define fresh-syms (list (gensym (car syms))))
                   (list syms fresh-syms
                         (PolyRow-constraints t)
                         (PolyRow-body* fresh-syms t)))
                 (list nps freshp constrp bp)))])))



;;***************************************************************
;; Smart Constructors/Expanders for Class-related structs
;;***************************************************************


;; Row*
;; This is a custom constructor for Row types
;; Sorts all clauses by the key (the clause name)
(define (Row* inits fields methods augments init-rest)
  (make-Row inits
            (sort-row-clauses fields)
            (sort-row-clauses methods)
            (sort-row-clauses augments)
            init-rest))

;; Class*
;; This is a custom constructor for Class types that
;; doesn't require writing make-Row everywhere
(define/cond-contract (Class* row-var inits fields methods augments init-rest)
  (-> (or/c F? B? Row? #f)
      (listof (list/c symbol? Type? boolean?))
      (listof (list/c symbol? Type?))
      (listof (list/c symbol? Type?))
      (listof (list/c symbol? Type?))
      (or/c Type? #f)
      Class?)
  (make-Class row-var (Row* inits fields methods augments init-rest)))

;; Class:*
;; This match expander replaces the built-in matching with
;; a version that will merge the members inside the substituted row
;; with the existing fields.

;; helper function for the expansion of Class:*
;; just does the merging
(define (merge-class/row class-type)
  (define row (Class-row-ext class-type))
  (define class-row (Class-row class-type))
  (define inits (Row-inits class-row))
  (define fields (Row-fields class-row))
  (define methods (Row-methods class-row))
  (define augments (Row-augments class-row))
  (define init-rest (Row-init-rest class-row))
  (cond [(and row (Row? row))
         (define row-inits (Row-inits row))
         (define row-fields (Row-fields row))
         (define row-methods (Row-methods row))
         (define row-augments (Row-augments row))
         (define row-init-rest (Row-init-rest row))
         (list row
               ;; Init types from a mixin go at the start, since
               ;; mixins only add inits at the start
               (append row-inits inits)
               ;; FIXME: instead of sorting here every time
               ;;        the match expander is called, the row
               ;;        fields should be merged on substitution
               (sort-row-clauses (append fields row-fields))
               (sort-row-clauses (append methods row-methods))
               (sort-row-clauses (append augments row-augments))
               ;; The class type's existing init-rest types takes
               ;; precedence since it's the one that was already assumed
               ;; (say, in a mixin type's domain). The mismatch will
               ;; be caught by application type-checking later.
               (if init-rest init-rest row-init-rest))]
        [else (list row inits fields methods augments init-rest)]))

;; sorts the given field of a Row by the member name
(define (sort-row-clauses clauses)
  (sort clauses (λ (x y) (symbol<? (car x) (car y)))))

(define-match-expander Class:*
  (λ (stx)
    (syntax-case stx ()
      [(_ row-pat inits-pat fields-pat methods-pat augments-pat init-rest-pat)
       #'(? Class?
            (app merge-class/row
                 (list row-pat inits-pat fields-pat
                       methods-pat augments-pat init-rest-pat)))])))


;;***************************************************************
;; Special Name Expanders
;;***************************************************************


;; alternative to Name: that only matches the name part
(define-match-expander Name/simple:
  (λ (stx)
    (syntax-parse stx
      [(_ name-pat) #'(Name: name-pat _ _)])))

;; alternative to Name: that only matches struct names
(define-match-expander Name/struct:
  (λ (stx)
    (syntax-parse stx
      [(_) #'(Name: _ _ #t)]
      [(_ name-pat) #'(Name: name-pat _ #t)])))
