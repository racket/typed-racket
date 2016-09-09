#lang racket/base

;; This module provides type representations and utility functions
;; and pattern matchers on types

(require "../utils/utils.rkt")

;; TODO use contract-req
(require (utils tc-utils)
         "rep-utils.rkt"
         "core-rep.rkt"
         "values-rep.rkt"
         "type-mask.rkt"
         "object-rep.rkt"
         "free-variance.rkt"
         racket/match racket/list racket/set
         racket/contract
         racket/lazy-require
         racket/promise
         syntax/parse/define
         (for-syntax racket/base
                     racket/syntax
                     syntax/parse))

(provide (all-from-out "core-rep.rkt")
         Type?
         Mu-name:
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
         type-equal?
         Name/simple: Name/struct:
         unfold
         Union?
         Union:
         Union-elems
         (rename-out [make-Union* make-Union]
                     [Class:* Class:]
                     [Class* make-Class]
                     [Row* make-Row]
                     [Mu:* Mu:]
                     [Poly:* Poly:]
                     [PolyDots:* PolyDots:]
                     [PolyRow:* PolyRow:]
                     [Mu* make-Mu]
                     [Poly* make-Poly]
                     [PolyDots* make-PolyDots]
                     [PolyRow* make-PolyRow]
                     [Mu-body* Mu-body]
                     [Poly-body* Poly-body]
                     [PolyDots-body* PolyDots-body]
                     [PolyRow-body* PolyRow-body]))


(lazy-require
 ("../types/union.rkt" (Un))
 ("../types/overlap.rkt" (overlap?))
 ("../types/resolve.rkt" (resolve-app)))

(define name-table (make-weak-hasheq))

;; Name = Symbol

;; Type is defined in rep-utils.rkt

;; this is ONLY used when a type error ocurrs
(def-type Error () #:base)

;; de Bruijn indexes - should never appear outside of this file
;; bound type variables
;; i is an nat
(def-type B ([i natural-number/c]) #:base
  [#:intern-key i])

;; free type variables
;; n is a Name
(def-type F ([n symbol?])
  [#:intern-key n]
  [#:frees
   [#:vars (_) (single-free-var n)]
   [#:idxs (_) empty-free-vars]]
  [#:fold (_ #:self self) self]
  [#:walk (_) (void)])

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
  [#:intern-key (hash-id id)]
  [#:frees (f) empty-free-vars]
  [#:fold (_ #:self self) self]
  [#:walk (_) (void)]
  #:needs-resolving)

;; rator is a type
;; rands is a list of types
;; stx is the syntax of the pair of parens
(def-type App ([rator Type?]
               [rands (listof Type?)]
               [stx (or/c #f syntax?)])
  [#:intern-key (cons (Rep-seq rator) (map Rep-seq rands))]
  [#:frees (f)
   (match rator 
     [(Name: n _ _)
      (instantiate-frees n (map f rands))]
     [_ (f (resolve-app rator rands stx))])]
  [#:fold (f) (make-App (f rator)
                        (map f rands)
                        stx)]
  [#:walk (f) (begin (f rator) (for-each f rands))]
  #:needs-resolving)


;; name is a Symbol (not a Name)
;; contract is used when generating contracts from types
;; predicate is used to check (at compile-time) whether a value belongs
;; to that base type. This is used to check for subtyping between value
;; types and base types.
;; numeric determines if the type is a numeric type
(def-type Base ([name symbol?]
                [contract syntax?]
                [predicate procedure?]
                [numeric? boolean?])
  #:base
  [#:intern-key name] 
  [#:type-mask
   (if numeric?
       mask:number
       (case name
         [(Char) mask:char]
         [(String) mask:string]
         [(Void) mask:void]
         [(Symbol) mask:symbol]
         [else mask:base-other]))])


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
             #:with variance #'Covariant
             #:with fld-frees #'(frees name))
    (pattern [name:id #:contravariant]
             #:with variance #'Contravariant
             #:with fld-frees #'(flip-variances (frees name)))
    (pattern [name:id #:invariant]
             #:with variance #'Invariant
             #:with fld-frees #'(make-invariant (frees name))))
  (syntax-parse stx
    [(_ name:var-name ((~var flds (structural-flds #'frees)) ...) . rst)
     #'(def-rep name ([flds.name Type?] ...)
         [#:parent Type]
         [#:intern-key (list* (Rep-seq flds.name) ...)]
         [#:variances flds.variance ...]
         [#:frees (frees) (combine-frees (list flds.fld-frees ...))]
         [#:fold (f) (name.constructor (f flds.name) ...)]
         [#:walk (f) (begin (f flds.name) ...)]
         . rst)]))


;;--------
;; Pairs
;;--------

;; left and right are Types
(def-structural Pair ([left #:covariant]
                      [right #:covariant])
  [#:type-mask mask:pair])

;;----------------
;; Mutable Pairs
;;----------------

(def-type MPairTop () [#:type-mask mask:mpair] #:base)

;; *mutable* pairs - distinct from regular pairs
;; left and right are Types
(def-structural MPair ([left #:invariant] [right #:invariant])
  [#:type-mask mask:mpair]
  [#:top MPairTop?])

;;----------
;; Vectors
;;----------

(def-type VectorTop () [#:type-mask mask:vector] #:base)

;; elem is a Type
(def-structural Vector ([elem #:invariant])
  [#:type-mask mask:vector]
  [#:top VectorTop?])

;;------
;; Box
;;------

(def-type BoxTop ()
  [#:type-mask mask:box] #:base)

(def-structural Box ([elem #:invariant])
  [#:type-mask mask:box]
  [#:top BoxTop?])

;;----------
;; Channel
;;----------

(def-type ChannelTop ()
  [#:type-mask mask:channel] #:base)

(def-structural Channel ([elem #:invariant])
  [#:type-mask mask:channel]
  [#:top ChannelTop?])

;;----------------
;; Async-Channel
;;----------------

(def-type Async-ChannelTop ()
  [#:type-mask mask:channel] #:base)

(def-structural Async-Channel ([elem #:invariant])
  [#:type-mask mask:channel]
  [#:top Async-ChannelTop?])

;;-------------
;; ThreadCell
;;-------------

(def-type ThreadCellTop ()
  [#:type-mask mask:thread-cell] #:base)

(def-structural ThreadCell ([elem #:invariant])
  [#:type-mask mask:thread-cell]
  [#:top ThreadCellTop?])

;;----------
;; Promise
;;----------

(def-structural Promise ([elem #:covariant])
  [#:type-mask mask:promise])

;;------------
;; Ephemeron
;;------------

(def-structural Ephemeron ([elem #:covariant])
  [#:type-mask mask:ephemeron])


;;-----------
;; Weak-Box
;;-----------

(def-type Weak-BoxTop ()
  [#:type-mask mask:other-box] #:base)

(def-structural Weak-Box ([elem #:invariant])
  [#:type-mask mask:other-box]
  [#:top Weak-BoxTop?])


;;---------------
;; CustodianBox
;;---------------

(def-structural CustodianBox ([elem #:covariant])
  [#:type-mask mask:other-box])

;;------
;; Set
;;------

;; TODO separate mutable/immutable set types
(def-structural Set ([elem #:covariant])
  [#:type-mask mask:set])

;;------------
;; Hashtable
;;------------

(def-type HashtableTop ()
  [#:type-mask mask:hash] #:base)

;; TODO separate mutable/immutable Hashtables
(def-structural Hashtable ([key #:invariant] [value #:invariant])
  [#:type-mask mask:hash]
  [#:top HashtableTop?])


;;------
;; Evt
;;------

(def-structural Evt ([result #:covariant]))

;;--------
;; Param
;;--------

(def-structural Param ([in #:contravariant]
                       [out #:covariant])
  [#:type-mask mask:procedure])


;;---------
;; Syntax
;;---------

;; t is the type of the result of syntax-e, not the result of syntax->datum
(def-structural Syntax ([t #:covariant])
  [#:type-mask mask:syntax])

;;---------
;; Future
;;---------

(def-structural Future ([t #:covariant])
  [#:type-mask mask:future])


;;---------------
;; Prompt-Tagof
;;---------------

(def-type Prompt-TagTop ()
  [#:type-mask mask:prompt-tag] #:base)

;; body: the type of the body
;; handler: the type of the prompt handler
;;   prompts with this tag will return a union of `body` 
;;   and the codomains of `handler`
(def-structural Prompt-Tagof ([body #:invariant]
                              [handler #:invariant])
  [#:type-mask mask:prompt-tag]
  [#:top Prompt-TagTop?])

;;--------------------------
;; Continuation-Mark-Keyof
;;--------------------------

(def-type Continuation-Mark-KeyTop ()
  [#:type-mask mask:continuation-mark-key] #:base)

;; value: the type of allowable values
(def-structural Continuation-Mark-Keyof ([value #:invariant])
  [#:type-mask mask:continuation-mark-key]
  [#:top Continuation-Mark-KeyTop?])

;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
;; List/Vector Types (that are not simple structural types)
;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 

;; dotted list -- after expansion, becomes normal Pair-based list type
(def-type ListDots ([dty Type?] [dbound (or/c symbol? natural-number/c)])
  [#:intern-key (cons (Rep-seq dty) dbound)]
  [#:frees
   [#:vars (f)
    (if (symbol? dbound)
        (free-vars-remove (f dty) dbound)
        (f dty))]
   [#:idxs (f)
    (if (symbol? dbound)
        (combine-frees (list (single-free-var dbound) (f dty)))
        (f dty))]]
  [#:fold (f) (make-ListDots (f dty) dbound)]
  [#:walk (f) (f dty)])



;; elems are all Types
(def-type HeterogeneousVector ([elems (listof Type?)])
  [#:intern-key (map Rep-seq elems)]
  [#:frees (f) (make-invariant (combine-frees (map f elems)))]
  [#:fold (f) (make-HeterogeneousVector (map f elems))]
  [#:walk (f) (for-each f elems)]
  [#:type-mask mask:vector]
  [#:top VectorTop?])


;; * * * * * * *
;; Type Binders
;; * * * * * * *


(def-type Mu ([body Type?])
  #:no-provide
  [#:intern-key (Rep-seq body)]
  [#:frees (f) (f body)]
  [#:fold (f) (make-Mu (f body))]
  [#:walk (f) (f body)]
  [#:type-mask (Type-mask body)]
  #:needs-resolving)

;; n is how many variables are bound here
;; body is a type
(def-type Poly ([n exact-nonnegative-integer?]
                [body Type?])
  #:no-provide
  [#:intern-key (cons n (Rep-seq body))]
  [#:frees (f) (f body)]
  [#:fold (f) (make-Poly n (f body))]
  [#:walk (f) (f body)]
  [#:type-mask (Type-mask body)])

;; n is how many variables are bound here
;; there are n-1 'normal' vars and 1 ... var
(def-type PolyDots ([n exact-nonnegative-integer?]
                    [body Type?])
  #:no-provide
  [#:intern-key (cons n (Rep-seq body))]
  [#:frees (f) (f body)]
  [#:fold (f) (make-PolyDots n (f body))]
  [#:walk (f) (f body)]
  [#:type-mask (Type-mask body)])

;; interp. A row polymorphic function type
;; constraints are row absence constraints, represented
;; as a set for each of init, field, methods
(def-type PolyRow ([constraints (list/c list? list? list? list?)]
                   [body Type?])
  #:no-provide
  [#:intern-key (cons (Rep-seq body) constraints)]
  [#:frees (f) (f body)]
  [#:fold (f) (make-PolyRow constraints (f body))]
  [#:walk (f) (f body)]
  [#:type-mask (Type-mask body)])

;; pred : identifier
(def-type Opaque ([pred identifier?]) #:base
  [#:intern-key (hash-id pred)])



;; kw : keyword?
;; ty : Type
;; required? : Boolean
(def-rep Keyword ([kw keyword?] [ty Type?] [required? boolean?])
  [#:intern-key (vector-immutable kw (Rep-seq ty) required?)]
  [#:frees (f) (f ty)]
  [#:fold (f) (make-Keyword kw (f ty) required?)]
  [#:walk (f) (f ty)])

(def-rep arr ([dom (listof Type?)]
              [rng SomeValues?]
              [rest (or/c #f Type?)]
              [drest (or/c #f (cons/c Type? (or/c natural-number/c symbol?)))]
              [kws (listof Keyword?)])
  [#:intern-key (vector-immutable
                 (map Rep-seq dom) (Rep-seq rng) (and rest (Rep-seq rest))
                 (and drest (cons (Rep-seq (car drest)) (cdr drest)))
                 (map Rep-seq kws))]
  [#:frees
   [#:vars (f)
    (combine-frees
     (append (map (compose flip-variances f)
                  (append (if rest (list rest) null)
                          (map Keyword-ty kws)
                          dom))
             (match drest
               [(cons t (? symbol? bnd))
                (list (free-vars-remove (flip-variances (f t)) bnd))]
               [(cons t _)
                (list (flip-variances (f t)))]
               [_ null])
             (list (f rng))))]
   [#:idxs (f)
    (combine-frees
     (append (map (compose flip-variances f)
                  (append (if rest (list rest) null)
                          (map Keyword-ty kws)
                          dom))
             (match drest
               [(cons t (? symbol? bnd))
                (list (single-free-var bnd Contravariant)
                      (flip-variances (f t)))]
               [(cons t _)
                (list (flip-variances (f t)))]
               [_ null])
             (list (f rng))))]]
  [#:fold (f) (make-arr (map f dom)
                        (f rng)
                        (and rest (f rest))
                        (and drest (cons (f (car drest)) (cdr drest)))
                        (map f kws))]
  [#:walk (f)
   (begin (for-each f dom)
          (f rng)
          (when drest (f (car drest)))
          (when rest (f rest))
          (for-each f kws))])

;; arities : Listof[arr]
(def-type Function ([arities (listof arr?)])
  [#:intern-key (map Rep-seq arities)]
  [#:type-mask mask:procedure]
  [#:frees (f) (combine-frees (map f arities))]
  [#:fold (f) (make-Function (map f arities))]
  [#:walk (f) (for-each f arities)])


(def-rep fld ([t Type?] [acc identifier?] [mutable? boolean?])
  [#:intern-key (cons (hash-id acc) (Rep-seq t))]
  [#:frees (f) (if mutable? (make-invariant (f t)) (f t))]
  [#:fold (f) (make-fld (f t) acc mutable?)]
  [#:walk (f) (f t)])

;; name : identifier
;; parent : Struct
;; flds : Listof[fld]
;; proc : Function Type
;; poly? : is this type polymorphically variant
;;         If not, then the predicate is enough for higher order checks
;; pred-id : identifier for the predicate of the struct
;; acc-ids : names of the accessors
;; maker-id : name of the constructor
(def-type Struct ([name identifier?]
                  [parent (or/c #f Struct?)]
                  [flds (listof fld?)]
                  [proc (or/c #f Function?)]
                  [poly? boolean?]
                  [pred-id identifier?])
  [#:intern-key (cons (hash-id name) (map Rep-seq flds))]
  [#:frees (f) (combine-frees (map f (append (if proc (list proc) null)
                                             (if parent (list parent) null)
                                             flds)))]
  [#:fold (f) (make-Struct name
                           (and parent (f parent))
                           (map f flds)
                           (and proc (f proc))
                           poly?
                           pred-id)]
  [#:walk (f)
   (begin (f parent)
          (for-each f flds)
          (f proc))]
  ;; This should eventually be based on understanding of struct properties.
  [#:type-mask (mask-union mask:struct mask:procedure)])

;; Represents prefab structs
;; key  : prefab key encoding mutability, auto-fields, etc.
;; flds : the types of all of the prefab fields
(def-type Prefab ([key prefab-key?]
                  [flds (listof Type?)])
  [#:intern-key (cons key (map Rep-seq flds))]
  [#:frees (f) (combine-frees (map f flds))]
  [#:fold (f) (make-Prefab key (map f flds))]
  [#:walk (f) (for-each f flds)]
  [#:type-mask mask:prefab])

(def-type StructTypeTop ()
  #:base
  [#:type-mask mask:struct-type])

;; A structure type descriptor
(def-type StructType ([s (or/c F? B? Struct? Prefab?)])
  [#:intern-key (Rep-seq s)]
  [#:frees (f) (f s)]
  [#:fold (f) (make-StructType (f s))]
  [#:walk (f) (f s)]
  [#:type-mask mask:struct-type]
  [#:top StructTypeTop?])

(def-type StructTop ([name Struct?])
  [#:intern-key (Rep-seq name)]
  [#:frees (f) (f name)]
  [#:fold (f) (make-StructTop (f name))]
  [#:walk (f) (f name)]
  [#:type-mask (mask-union mask:struct mask:procedure)])




;; v : Racket Value
;; contract will change to the following after
;; base types are redone:
(def-type Value ([val any/c])
  #:base
  [#:intern-key val]
  [#:type-mask
   (match val
     [(? number?) mask:number]
     [#t mask:true]
     [#f mask:false]
     [(? symbol?) mask:symbol]
     [(? string?) mask:string]
     [(? char?) mask:char]
     [(? null?) mask:null]
     [(? void?) mask:void]
     [_ mask:unknown])])

;; elems : Listof[Type]
(def-type Union ([elems (and/c (listof Type?) (length>=/c 2))])
  #:no-provide
  [#:intern-key (for/hash ([elem (in-list elems)]) (values elem #t))]
  [#:frees (f) (combine-frees (map f elems))]
  [#:fold (f) (apply Un (map f elems))]
  [#:walk (f) (for-each f elems)]
  [#:type-mask
   (for/fold ([mask mask:bottom])
             ([elem (in-list elems)])
     (mask-union mask (Type-mask elem)))])

(define (make-Union* elems)
  (match elems
    [(list) (make-Bottom)]
    [(list t) t]
    [_ (make-Union elems)]))

;; Intersection
(def-type Intersection ([elems (and/c (listof Type?) (length>=/c 2))])
  [#:intern-key (for/hash ([elem (in-list elems)]) (values elem #t))]
  [#:frees (f) (combine-frees (map f elems))]
  [#:fold (f) (apply -unsafe-intersect (map f elems))]
  [#:walk (f) (for-each f elems)]
  [#:type-mask
   (for/fold ([mask mask:unknown])
             ([elem (in-list elems)])
     (mask-intersect mask (Type-mask elem)))])

;;  constructor for intersections
;; in general, intersections should be built
;; using the 'intersect' operator, which worries
;; about actual subtyping, etc...
(define (-unsafe-intersect . ts)
  (let loop ([elems (set)]
             [ts ts])
    (match ts
      [(list)
       (cond
         [(set-empty? elems) (make-Univ)]
         ;; size = 1 ?
         [(= 1 (set-count elems)) (set-first elems)]
         ;; size > 1, build an intersection
         [else (make-Intersection (set->list elems))])]
      [(cons t ts)
       (match t
         [(? Bottom?) t]
         [(Univ:) (loop elems ts)]
         [(Intersection: ts*) (loop elems (append ts* ts))]
         [t (cond
              [(for/or ([elem (in-immutable-set elems)]) (not (overlap? elem t)))
               (make-Bottom)]
              [else (loop (set-add elems t) ts)])])])))


(def-type Refinement ([parent Type?] [pred identifier?])
  [#:intern-key (cons (hash-id pred) (Rep-seq parent))]
  [#:frees (f) (f parent)]
  [#:fold (f) (make-Refinement (f parent) pred)]
  [#:walk (f) (f parent)]
  [#:type-mask (Type-mask parent)])

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
  [#:intern-key
   (let ([intern (λ (l) (list-update l 1 Rep-seq))])
     (list (map intern inits)
           (map intern fields)
           (map intern methods)
           (map intern augments)
           (and init-rest (Rep-seq init-rest))))]
  [#:frees (f)
   (let ([extract-frees (λ (l) (f (second l)))])
     (combine-frees
      (append (map extract-frees inits)
              (map extract-frees fields)
              (map extract-frees methods)
              (map extract-frees augments)
              (if init-rest (list (f init-rest)) null))))]
  [#:fold (f)
   (let ([update (λ (l) (list-update l 1 f))])
     (make-Row (map update inits)
               (map update fields)
               (map update methods)
               (map update augments)
               (and init-rest (f init-rest))))]
  [#:walk (f)
   (let ([walk (λ (l) (f (second l)))])
     (begin (for-each walk inits)
            (for-each walk fields)
            (for-each walk methods)
            (for-each walk augments)
            (when init-rest (f init-rest))))])

(def-type ClassTop ()
  #:base
  [#:type-mask mask:class])

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
  [#:intern-key (cons (and row-ext (Rep-seq row-ext)) (Rep-seq row))]
  [#:frees (f)
   (combine-frees
    (append (if row-ext (list (f row-ext)) null)
            (list (f row))))]
  [#:fold (f) (make-Class (and row-ext (f row-ext))
                          (f row))]
  [#:walk (f) (begin (when row-ext (f row-ext))
                     (f row))]
  [#:type-mask mask:class]
  [#:top ClassTop?])


;;--------------------------
;; Instance (of a class)
;;--------------------------

(def-type Instance ([cls Type?])
  [#:intern-key (Rep-seq cls)]
  [#:frees (f) (f cls)]
  [#:fold (f) (make-Instance (f cls))]
  [#:walk (f) (f cls)]
  [#:type-mask mask:instance])


;; interp:
;; name is the id of the signature
;; extends is the extended signature or #f
;; mapping maps variables in a signature to their types
;; This is not a type because signatures do not correspond to any values
(def-rep Signature ([name identifier?]
                    [extends (or/c identifier? #f)]
                    [mapping (listof (cons/c identifier? Type?))])
  [#:intern-key (hash-id name)]
  [#:frees (f) (combine-frees (map (match-lambda
                                     [(cons _ t) (f t)])
                                   mapping))]
  [#:fold (f) (make-Signature name extends (map (match-lambda
                                                  [(cons id t) (cons id (f t))])
                                                mapping))]
  [#:walk (f) (make-Signature name extends (for-each (match-lambda
                                                       [(cons id t) (cons id (f t))])
                                                     mapping))])


(def-type UnitTop ()
  #:base
  [#:type-mask mask:unit])


;; interp: imports is the list of imported signatures
;;         exports is the list of exported signatures
;;         init-depends is the list of init-depend signatures
;;         result is the type of the body of the unit
(def-type Unit ([imports (listof Signature?)]
                [exports (listof Signature?)]
                [init-depends (listof Signature?)]
                [result SomeValues?])
  [#:intern-key (list* (Rep-seq result)
                       (map Rep-seq imports)
                       (map Rep-seq exports)
                       (map Rep-seq init-depends))]
  [#:frees (f) (f result)]
  [#:fold (f) (make-Unit (map f imports)
                         (map f exports)
                         (map f init-depends)
                         (f result))]
  [#:walk (f) (begin (for-each f imports)
                     (for-each f exports)
                     (for-each f init-depends)
                     (f result))]
  [#:type-mask mask:unit]
  [#:top UnitTop?])

;; sequences
;; includes lists, vectors, etc
;; tys : sequence produces this set of values at each step
(def-type Sequence ([tys (listof Type?)])
  [#:intern-key (map Rep-seq tys)]
  [#:frees (f) (combine-frees (map f tys))]
  [#:fold (f) (make-Sequence (map f tys))]
  [#:walk (f) (for-each f tys)])

;; Distinction
;; comes from define-new-subtype
;; nm: a symbol representing the name of the type
;; id: a symbol created with gensym
;; ty: a type for the representation (i.e. each distinction
;;     is a subtype of its ty)
(def-type Distinction ([nm symbol?] [id symbol?] [ty Type?])
  [#:intern-key (list* nm id (Rep-seq ty))]
  [#:frees (f) (f ty)]
  [#:fold (f) (make-Distinction nm id (f ty))]
  [#:walk (f) (f ty)]
  [#:type-mask (Type-mask ty)])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;************************************************************
;; Helpers: Type Variable Abstraction/Instantiation
;;************************************************************

;; used for abstracting/instantiating type variables
(define (type-binder-transform f start abstracting?)
  (define dbound-fn (if abstracting? values F-n))
  (define not-abstracting? (not abstracting?))
  (let rec/lvl ([cur start] [lvl 0])
    (let rec ([cur cur])
      (match cur
        [(F: name*) #:when abstracting? (f name* make-B lvl cur)]
        [(B: idx) #:when not-abstracting? (f idx (λ (x) x) lvl cur)]
        [(Union: elems)
         ;; prevents duplicates, which apparently is needed to avoid
         ;; infinite loops here...?
         (define seen (make-hasheq))
         (define ts
           (for*/fold ([ts null])
                      ([elem (in-list elems)]
                       [elem (in-value (rec elem))]
                       [seq (in-value (Rep-seq elem))])
             (cond
               [(hash-ref seen seq #f) ts]
               [else (hash-set! seen seq #t)
                     (cons elem ts)])))
         (match ts
           [(list) (make-Bottom)]
           [(list t) t]
           [_ (make-Union ts)])]
        [(arr: dom rng rest drest kws)
         (make-arr (map rec dom)
                   (rec rng)
                   (and rest (rec rest))
                   (if drest
                       (cons (rec (car drest))
                             (let ([c (cdr drest)])
                               (f c dbound-fn lvl c)))
                       #f)
                   (map rec kws))]
        [(Mu: body) (make-Mu (rec/lvl body (add1 lvl)))]
        [(ValuesDots: rs dty dbound)
         (make-ValuesDots (map rec rs)
                          (rec dty)
                          (f dbound dbound-fn lvl dbound))]
        [(ListDots: dty dbound)
         (make-ListDots (rec dty)
                        (f dbound dbound-fn lvl dbound))]
        [(PolyRow: constraints body)
         (make-PolyRow constraints (rec/lvl body (add1 1 lvl)))]
        [(PolyDots: n body)
         (make-PolyDots n (rec/lvl body (+ n lvl)))]
        [(Poly: n body)
         (make-Poly n (rec/lvl body (+ n lvl)))]
        [_ (Rep-fold rec cur)]))))

(define/cond-contract (abstract-many names ty)
  (-> (listof symbol?) Type? Type?)
  (define n (length names))
  (define mapping (for/list ([nm (in-list names)]
                             [i (in-range n 0 -1)])
                    (cons nm (sub1 i))))
  ;; transform : symbol (Integer -> a) a -> a
  ;; apply `mapping` to `name*`, returning `default` if it's not there
  ;; use `f` to wrap the result
  ;; note that this takes into account the value of the outer lvl
  (define (transform name* fn lvl default)
    (cond [(assq name* mapping)
           => (λ (pr) (fn (+ (cdr pr) lvl)))]
          [else default]))
  (type-binder-transform transform ty #t))


(define/cond-contract (instantiate-many images ty)
  (-> (listof Type?) Type? Type?)
  (define n (length images))
  (define mapping (for/list ([img (in-list images)]
                             [i (in-range n 0 -1)])
                    (cons (sub1 i) img)))
  ;; transform : Integer (Name -> a) a -> a
  ;; apply `mapping` to `n`, returning `default` if it's not there
  ;; use `f` to wrap the result
  ;; note that this takes into account the value of the outer `lvl`
  (define (transform n fn lvl default)
    (cond [(assf (λ (v) (eqv? (+ v lvl) n)) mapping)
           => (λ (pr) (fn (cdr pr)))]
          [else default]))
  (type-binder-transform transform ty #f))

(define (abstract name ty)
  (abstract-many (list name) ty))

(define (instantiate type sc)
  (instantiate-many (list type) sc))

;; the 'smart' constructor
(define (Mu* name body)
  (let ([v (make-Mu (abstract name body))])
    (hash-set! name-table v name)
    v))

;; the 'smart' destructor
(define (Mu-body* name t)
  (match t
    [(Mu: body)
     (instantiate (make-F name) body)]))

;; the 'smart' constructor
;;
;; Corresponds to closing a type in locally nameless representation
;; (turns free `names` into bound De Bruijn vars)
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
      (let ([v (make-Poly (length names) (abstract-many names body))])
        (hash-set! name-table v orig)
        v)))

;; the 'smart' destructor
(define (Poly-body* names t)
  (match t
    [(Poly: n body)
     (unless (= (length names) n)
       (int-err "Wrong number of names: expected ~a got ~a" n (length names)))
     (instantiate-many (map make-F names) body)]))

;; the 'smart' constructor
(define (PolyDots* names body)
  (if (null? names) body
      (let ([v (make-PolyDots (length names) (abstract-many names body))])
        (hash-set! name-table v names)
        v)))

;; the 'smart' destructor
(define (PolyDots-body* names t)
  (match t
    [(PolyDots: n body)
     (unless (= (length names) n)
       (int-err "Wrong number of names: expected ~a got ~a" n (length names)))
     (instantiate-many (map make-F names) body)]))

;; Constructor and destructor for row polymorphism
;;
;; Note that while `names` lets you specify multiple names, it's
;; expected that row polymorphic types only bind a single name at
;; a time. This may change in the future.
;;
(define (PolyRow* names constraints body #:original-names [orig names])
  (let ([v (make-PolyRow constraints (abstract-many names body))])
    (hash-set! name-table v orig)
    v))

(define (PolyRow-body* names t)
  (match t
    [(PolyRow: constraints body)
     (instantiate-many (map make-F names) body)]))

(print-struct #t)

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

(define-match-expander Mu-name:
  (lambda (stx)
    (syntax-case stx ()
      [(_ np bp)
       #'(? Mu?
            (app (lambda (t) (let ([sym (hash-ref name-table t (lambda _ (gensym)))])
                               (list sym (Mu-body* sym t))))
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
                          [syms (hash-ref name-table t (lambda _ (build-list n (lambda _ (gensym)))))])
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
                          [syms (hash-ref name-table t (lambda _ (build-list n (lambda _ (gensym)))))]
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
                          [syms (hash-ref name-table t (lambda _ (build-list n (lambda _ (gensym)))))])
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
                   (define syms (hash-ref name-table t (λ _ (list (gensym)))))
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
                   (define syms (hash-ref name-table t (λ _ (list (gensym)))))
                   (define fresh-syms (list (gensym (car syms))))
                   (list syms fresh-syms
                         (PolyRow-constraints t)
                         (PolyRow-body* fresh-syms t)))
                 (list nps freshp constrp bp)))])))

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


;; unfold : Type -> Type
;; must be applied to a Mu
(define/cond-contract (unfold t)
  (Mu? . -> . Type?)
  (match t
    [(Mu-unsafe: body) (instantiate t body)]
    [t (error 'unfold "not a mu! ~a" t)]))
