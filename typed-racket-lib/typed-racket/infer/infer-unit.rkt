#lang racket/unit

;; This is the main file that defines local type inference in TR
;;
;; The algorithm is based on
;;   "Local Type Inference" by Pierce and Turner
;;   ACM TOPLAS, Vol. 22, No. 1, January 2000.
;;

(require "../utils/utils.rkt"
         (except-in
          (combine-in
           (utils tc-utils prefab identifier)
           (rep free-variance type-rep prop-rep object-rep
                values-rep rep-utils type-mask)
           (types utils abbrev numeric-tower subtype resolve
                  substitute generalize)
           (env lexical-env index-env tvar-env)
           (logic proves))
          make-env -> ->* one-of/c)
         "constraint-structs.rkt"
         "signatures.rkt" "fail.rkt"
         "promote-demote.rkt"
         racket/match
         ;racket/trace
         (contract-req)
         (for-syntax
           racket/base
           syntax/parse)
         racket/hash racket/list racket/stream)

(import dmap^ constraints^)
(export infer^)

;; For more data definitions, see "constraint-structs.rkt"
;;
;; A Seen is a set represented by a list of Pair<Seq, Seq>
(define (empty-set) '())

(define current-seen (make-parameter (empty-set)))

;; Type Type -> Pair<Seq, Seq>
;; construct a pair for the set of seen type pairs
(define seen-before cons)

;; Context, contains which type variables and indices to infer and which cannot be mentioned in
;; constraints.
(define-struct/cond-contract context
  ([bounds (listof symbol?)]
   [vars (listof symbol?)]
   [indices (listof symbol?)]) #:transparent)

(define (context-add-vars ctx vars)
  (match ctx
    [(context V X Y)
     (context V (append vars X) Y)]))

(define (context-add-var ctx var)
  (match ctx
    [(context V X Y)
     (context V (cons var X) Y)]))

(define (context-add ctx #:bounds [bounds empty] #:vars [vars empty] #:indices [indices empty])
  (match ctx
    [(context V X Y)
     (context (append bounds V) (append vars X) (append indices Y))]))

(define (inferable-index? ctx bound)
  (match ctx
    [(context _ _ Y)
     (memq bound Y)]))

(define ((inferable-var? ctx) var)
  (match ctx
    [(context _ X _)
     (memq var X)]))

(define (empty-cset/context ctx)
  (match ctx
    [(context _ X Y)
     (empty-cset X Y)]))




;; Type Type Seen -> Seen
;; Add the type pair to the set of seen type pairs
(define/cond-contract (remember s t A)
  ((or/c AnyValues? Values/c ValuesDots?) (or/c AnyValues? Values/c ValuesDots?)
   (listof (cons/c Rep? Rep?))
   . -> .
   (listof (cons/c Rep? Rep?)))
 (cons (seen-before s t) A))

;; Type Type -> Boolean
;; Check if a given type pair have been seen before
(define/cond-contract (seen? s t cs)
  ((or/c AnyValues? Values/c ValuesDots?) (or/c AnyValues? Values/c ValuesDots?)
   (listof (cons/c Rep? Rep?))
   . -> . any/c)
 (member (seen-before s t) cs))

;; (CMap DMap -> Pair<CMap, DMap>) CSet -> CSet
;; Map a function over a constraint set
(define (map/cset f cset)
  (% make-cset (for/list/fail ([cmap/dmap (in-stream (cset-maps cset))])
                 (f (car cmap/dmap) (cdr cmap/dmap)))))

;; Symbol DCon -> DMap
;; Construct a dmap containing only a single mapping
(define (singleton-dmap dbound dcon)
  (make-dmap (make-immutable-hash (list (cons dbound dcon)))))

;; Hash<K, V> Listof<K> -> Hash<K, V>
;; Remove all provided keys from the hash table
(define (hash-remove* hash keys)
  (for/fold ([h hash]) ([k (in-list keys)]) (hash-remove h k)))

(define (mover cset dbound vars f)
  (map/cset
   (lambda (cmap dmap)
     (when (hash-has-key? (dmap-map dmap) dbound)
       (int-err "Tried to move vars to dbound that already exists"))
     (% cons
        (hash-remove* cmap (cons dbound vars))
        (dmap-meet
         (singleton-dmap
          dbound
          (f cmap))
         dmap)))
   cset))

;; dbound : index variable
;; vars : listof[type variable] - temporary variables
;; cset : the constraints being manipulated
;; takes the constraints on vars and creates a dmap entry constraining dbound to be |vars|
;; with the constraints that cset places on vars
(define/cond-contract (move-vars-to-dmap cset dbound vars)
  (cset? symbol? (listof symbol?) . -> . cset?)
  (mover cset dbound vars
         (λ (cmap)
           (make-dcon (for/list ([v (in-list vars)])
                        (hash-ref cmap v
                                  (λ () (int-err "No constraint for new var ~a" v))))
                      #f))))

;; cset : the constraints being manipulated
;; var : index variable being inferred
;; dbound : constraining index variable
;;
(define/cond-contract (move-dotted-rest-to-dmap cset var dbound)
  (cset? symbol? symbol? . -> . cset?)
  (mover cset var null
         (λ (cmap)
           (make-dcon-dotted
            null
            (hash-ref cmap var
                      (λ () (int-err "No constraint for bound ~a" var)))
            dbound))))

;; cset : the constraints being manipulated
;; vars : the variables that are the prefix of the dbound
;; dbound : index variable
(define/cond-contract (move-vars+rest-to-dmap cset vars dbound #:exact [exact? #f])
  ((cset? (listof symbol?) symbol?) (#:exact boolean?) . ->* . cset?)
  (mover cset dbound vars
         (λ (cmap)
           ((if exact? make-dcon-exact make-dcon)
            (for/list ([v (in-list vars)])
              (hash-ref cmap v no-constraint))
            (hash-ref cmap dbound (λ () (int-err "No constraint for bound ~a" dbound)))))))

;; Represents a sequence of types. types are the fixed prefix, and end is the remaining types
;; This is a unification of all of the dotted types that exist ListDots, ->..., and ValuesDots.
;; This allows for one implementation of the cgen algorithm for dotted types to be shared across all
;; of them.
(struct seq (types end) #:transparent)
(struct null-end () #:transparent)
(define -null-end (null-end))
;; ts is the pattern of the rest of the seq that can
;; occur 0 or more times
;; e.g. a rest argument of Num would just be (list Num)
;;      a rest arg of (Num Str) would be (list Num Str)
(struct star-end (ts) #:transparent)
(struct dotted-end (type bound) #:transparent)

(define (Values->seq v)
  (match v
    [(Values: ts) (seq ts -null-end)]
    [(ValuesDots: ts dty dbound) (seq ts (dotted-end (-result dty) dbound))]
    [_ #f]))


(define (List->end v)
  (match v
    [(== -Null) -null-end]
    [(Listof: t) (star-end (list t))]
    [(ListDots: t dbound) (dotted-end t dbound)]
    [_ #f]))

(define (List->seq v)
  (match v
    [(List: ts #:tail (app List->end end)) (and end (seq ts end))]
    [_ #f]))


(define (Sequence->seq v)
  (match v
    [(Sequence: ts) (seq ts -null-end)]
    [(SequenceDots: ts dty dbound) (seq ts (dotted-end dty dbound))]
    [_ #f]))


(define-match-expander ValuesSeq:
  (lambda (stx)
    (syntax-parse stx
      [(_ seq) #'(app Values->seq (? values seq))])))

(define-match-expander ListSeq:
  (lambda (stx)
    (syntax-parse stx
      [(_ seq) #'(app List->seq (? values seq))])))

(define-match-expander SequenceSeq:
  (lambda (stx)
    (syntax-parse stx
      [(_ seq) #'(app Sequence->seq (? values seq))])))


;; generate-dbound-prefix: Symbol Type? Natural (U Symbol #f) -> (Values (Listof Symbol) (Listof Type?))
;; Substitutes n fresh new variables, replaces dotted occurences of v in t with the variables (and
;; maybe new-end), and then for each variable substitutes it in for regular occurences of v.
(define (generate-dbound-prefix v ty n new-end)
  (define vars (build-list n (lambda (x) (gensym v))))
  (define ty* (substitute-dots (map make-F vars) (and new-end (make-F new-end)) v ty))
  (values
    vars
    (for/list ([var (in-list vars)])
      (substitute (make-F var) v ty*))))


(define/cond-contract (cgen/prop context p q)
  (context? Prop? Prop? . -> . (or/c #f cset?))
  (match* (p q)
    [(p p) (empty-cset/context context)]
    [(p (TrueProp:)) (empty-cset/context context)]
    [((FalseProp:) q) (empty-cset/context context)]
    ;; FIXME - is there something to be said about the logical ones?
    [((TypeProp: o s) (TypeProp: o t)) (cgen/inv context s t)]
    [((NotTypeProp: o s) (NotTypeProp: o t)) (cgen/inv context s t)]
    [(_ _) #f]))

;; s and t must be *latent* prop sets
(define/cond-contract (cgen/prop-set context s t)
  (context? PropSet? PropSet? . -> . (or/c #f cset?))
  (match* (s t)
    [(e e) (empty-cset/context context)]
    [((PropSet: p+ p-) (PropSet: q+ q-))
     (% cset-meet (cgen/prop context p+ q+) (cgen/prop context p- q-))]
    [(_ _) #f]))

(define/cond-contract (cgen/object context s t)
  (context? OptObject? OptObject? . -> . (or/c #f cset?))
  (match* (s t)
    [(e e) (empty-cset/context context)]
    [(e (Empty:)) (empty-cset/context context)]
    ;; FIXME - do something here
    [(_ _) #f]))

(define/cond-contract (cgen/seq context s-seq t-seq [objs '()])
  (->* (context? seq? seq?)
       ((listof (or/c #f OptObject?)))
       (or/c #f cset?))
  (match*/early (s-seq t-seq)
    ;; The simplest case - both are null-end
    [((seq ss (null-end))
      (seq ts (null-end)))
      (cgen/list context ss ts objs)]
    ;; One is null-end the other is star-end
    [((seq ss (null-end))
      (seq ts (star-end t-rest)))
     (define ss-len (length ss))
     (define ts-len (length ts))
     #:return-unless (<= ts-len ss-len) #f
     (define fewer-args (- ss-len ts-len))
     (define cycle-len (length t-rest))
     #:return-unless (zero? (remainder fewer-args cycle-len)) #f
     (define repetitions (quotient fewer-args cycle-len))
     (define new-ts (append ts (repeat-list t-rest repetitions)))
     (cgen/list context ss new-ts objs)]
    [((seq ss (star-end _))
      (seq ts (null-end)))
     #f]
    ;; Both are star-end
    [((seq ss (star-end s-rest))
      (seq ts (and t-end (star-end t-rest))))
     (cgen/seq context
               (seq (append s-rest ss) -null-end)
               (seq (append t-rest ts) t-end)
               objs)]
    ;; dotted below, nothing above
    [((seq ss (dotted-end dty dbound))
      (seq ts (null-end)))
     #:return-unless (inferable-index? context dbound)
     #f
     #:return-unless (<= (length ss) (length ts))
     #f
     (define-values (vars new-tys) (generate-dbound-prefix dbound dty (- (length ts) (length ss)) #f))
     (define-values (ts-front ts-back) (split-at ts (length ss)))
     (define-values (objs-front objs-back)
       (if (<= (length objs) (length ss))
           (values objs '())
           (split-at objs (length ss))))
     (% cset-meet
        (cgen/list context ss ts-front objs-front)
        (% move-vars-to-dmap (cgen/list (context-add context #:vars vars) new-tys ts-back objs-back) dbound vars))]
    ;; dotted above, nothing below
    [((seq ss (null-end))
      (seq ts (dotted-end dty dbound)))
     #:return-unless (inferable-index? context dbound)
     #f
     #:return-unless (<= (length ts) (length ss))
     #f
     (define-values (vars new-tys) (generate-dbound-prefix dbound dty (- (length ss) (length ts)) #f))
     (define-values (ss-front ss-back) (split-at ss (length ts)))
     (define-values (objs-front objs-back)
       (if (<= (length objs) (length ts))
           (values objs '())
           (split-at objs (length ts))))
     (% cset-meet
        (cgen/list context ss-front ts objs-front)
        (% move-vars-to-dmap (cgen/list (context-add-vars context vars) ss-back new-tys objs-back) dbound vars))]

    ;; same dotted bound
    [((seq ss (dotted-end s-dty dbound))
      (seq ts (dotted-end t-dty dbound)))
     #:return-unless (= (length ss) (length ts))
     #f
     (% cset-meet
        (cgen/list context ss ts objs)
        (if (inferable-index? context dbound)
            (extend-tvars (list dbound)
              (% move-vars+rest-to-dmap (cgen (context-add-var context dbound) s-dty t-dty) null dbound))
            (cgen context s-dty t-dty)))]

    ;; bounds are different
    [((seq ss (dotted-end s-dty dbound))
      (seq ts (dotted-end t-dty dbound*)))
     #:when (inferable-index? context dbound)
     #:return-unless (= (length ss) (length ts)) #f
     #:return-when (inferable-index? context dbound*) #f
     (% cset-meet
        (cgen/list context ss ts objs)
        (extend-tvars (list dbound*)
          (% move-dotted-rest-to-dmap (cgen (context-add-var context dbound) s-dty t-dty) dbound dbound*)))]
    [((seq ss (dotted-end s-dty dbound))
      (seq ts (dotted-end t-dty dbound*)))
     #:return-unless (inferable-index? context dbound*) #f
     #:return-unless (= (length ss) (length ts)) #f
     (% cset-meet
        (cgen/list context ss ts objs)
        (extend-tvars (list dbound)
          (% move-dotted-rest-to-dmap (cgen (context-add-var context dbound*) s-dty t-dty) dbound* dbound)))]

    ;; * <: ...
    [((seq ss (star-end (list s-rest-ty)))
      (seq ts (dotted-end t-dty dbound)))
     #:return-unless (inferable-index? context dbound)
     #f
     #:return-unless (<= (length ts) (length ss))
     #f
     (define new-bound (gensym dbound))
     (define-values (vars new-tys)
       (generate-dbound-prefix dbound t-dty (- (length ss) (length ts))
                               new-bound))
     (define-values (ss-front ss-back) (split-at ss (length ts)))
     (define-values (objs-front objs-back)
       (if (<= (length objs) (length ts))
           (values objs '())
           (split-at objs (length ts))))
     (% cset-meet
        (cgen/list context ss-front ts objs-front)
        (% move-vars+rest-to-dmap
           (% cset-meet
              (cgen/list (context-add context
                                      #:bounds (list new-bound)
                                      #:vars vars
                                      #:indices (list new-bound))
                         ss-back
                         new-tys
                         objs-back)
              (cgen (context-add-var context dbound) s-rest-ty t-dty))
           vars dbound #:exact #t))]
    ;; TODO figure out how above code could be modified to support
    ;; star-end w/ a cycle of len > 1
    [((seq ss (star-end _))
      (seq ts (dotted-end _ _)))
     #f]

    [((seq ss (dotted-end s-dty dbound))
      (seq ts (star-end (list t-rest-ty))))
     (cond
       [(inferable-index? context dbound)
        (define new-bound (gensym dbound))
        (define length-delta (- (length ts) (length ss)))
        (define-values (vars new-tys)
          (generate-dbound-prefix dbound s-dty (max 0 length-delta) new-bound))
        (define-values (objs-front objs-back)
          (if (<= (length objs) (length ss))
              (values objs '())
              (split-at objs (length ss))))
        (% cset-meet
           (cgen/list context ss (if (positive? length-delta)
                                     (drop-right ts length-delta)
                                     (list-extend ss ts t-rest-ty))
                      objs-front)
           (% move-vars+rest-to-dmap
              (% cset-meet
                 (cgen/list (context-add context #:bounds (list new-bound) #:vars vars #:indices (list new-bound))
                            new-tys (take-right ts (max 0 length-delta))
                            objs-back)
                 (cgen (context-add-var context dbound) s-dty t-rest-ty))
              vars dbound))]
       [else
        (extend-tvars (list dbound)
                      (cgen/seq (context-add context #:bounds (list dbound))
                                (seq ss (star-end (list s-dty)))
                                t-seq
                                objs))])]
    [((seq ts (dotted-end _ _))
      (seq ss (star-end _)))
     #f]))

(define/cond-contract (cgen/arrow context s-arr t-arr)
  (context? Arrow? Arrow? . -> . (or/c #f cset?))
  (match* (s-arr t-arr)
    [((Arrow: ss s-rest s-kws s)
      (Arrow: ts t-rest t-kws t))
     (define (rest->end rest)
       (match rest
         [(Rest: rst-ts) (star-end rst-ts)]
         [(RestDots: ty dbound)
          (dotted-end ty dbound)]
         [_ -null-end]))

     (define s-seq (seq ss (rest->end s-rest)))
     (define t-seq (seq ts (rest->end t-rest)))
     (and
      ;; if all keywords are optional, then we can just treat
      ;; them like they aren't there (or if there are none)
      (for/and ([s-kw (in-list s-kws)])
        (not (Keyword-required? s-kw)))
      (null? t-kws)
      (% cset-meet
         (cgen context s t)
         (cgen/seq context t-seq s-seq)))]))

(define/cond-contract (cgen/flds context flds-s flds-t)
  (context? (listof fld?) (listof fld?)  . -> . (or/c #f cset?))
  (% cset-meet*
   (for/list/fail ([s (in-list flds-s)] [t (in-list flds-t)])
     (match* (s t)
       ;; mutable - invariant
       [((fld: s _ #t) (fld: t _ #t)) (cgen/inv context s t)]
       ;; immutable - covariant
       [((fld: s _ #f) (fld: t _ #f)) (cgen context s t)]))))

(define (cgen/inv context s t)
  (% cset-meet (cgen context s t) (cgen context t s)))

;; context : the context of what to infer/not infer
;; S : a type to be the subtype of T
;; T : a type
;; produces a cset which determines a substitution that makes S a subtype of T
;; implements the V |-_X S <: T => C judgment from Pierce+Turner, extended with
;; the index variables from the TOPLAS paper
(define/cond-contract (cgen context S T [obj #f])
  (->* (context? (or/c Values/c ValuesDots? AnyValues?)
                 (or/c Values/c ValuesDots? AnyValues?))
       ((or/c #f OptObject?))
       (or/c #F cset?))
  ;; useful quick loop
  (define/cond-contract (cg S T [obj #f])
   (->* (Type? Type?) ((or/c #f OptObject?))
        (or/c #f cset?))
   (cgen context S T obj))
  (define/cond-contract (cg/inv S T)
   (Type? Type? . -> . (or/c #f cset?))
   (cgen/inv context S T))
  ;; this places no constraints on any variables
  (define empty (empty-cset/context context))
  ;; this constrains just x (which is a single var)
  (define (singleton S x T)
    (insert empty x S T))
  ;; FIXME -- figure out how to use parameters less here
  ;;          subtyping doesn't need to use it quite as much
  (define cs (current-seen))
  ;; if we've been around this loop before, we're done (for rec types)
  (cond
    [(equal? S T) empty] ;; (CG-Refl)
    [(Univ? T) empty] ;; CG-Top
    [(seen? S T cs) empty]
    [else
     (parameterize (;; remember S and T, and obtain everything we've seen from the context
                    ;; we can't make this an argument since we may call back and forth with
                    ;; subtyping, for example
                    [current-seen (remember S T cs)])
       (match*/early
        (S T)
        ;; AnyValues
        [((AnyValues: p) (AnyValues: q))
         (cgen/prop context p q)]

        [((or (Values: (list (Result: _ psets _) ...))
              (ValuesDots: (list (Result: _ psets _) ...) _ _))
          (AnyValues: q))
         (if (null? psets)
             empty
             (cset-join
              (for*/list ([pset (in-list psets)]
                          [cs (in-value (% cset-meet
                                           (cgen/prop context (PropSet-thn pset) q)
                                           (cgen/prop context (PropSet-els pset) q)))]
                          #:when cs)
                cs)))]

        ;; check all non Type? first so that calling subtype is safe

        ;; check each element
        [((Result: s pset-s o-s)
          (Result: t pset-t o-t))
         (% cset-meet
            (cg s t o-s)
            (cgen/prop-set context pset-s pset-t)
            (cgen/object context o-s o-t))]

        ;; Values just delegate to cgen/seq, except special handling for -Bottom.
        ;; A single -Bottom in a Values means that there is no value returned and so any other
        ;; Values or ValuesDots should be above it.
        [((ValuesSeq: s-seq) (ValuesSeq: t-seq))
         ;; Check for a substition that S is below (ret -Bottom).
         (define bottom-case
           (match S
             [(Values: (list (Result: s _ o-s)))
              (cgen context s -Bottom o-s)]
             [else #f]))
         (define regular-case
           (cgen/seq context s-seq t-seq))
         ;; If we want the OR of the csets that the two cases return.
         (cset-join
          (filter values
                  (list bottom-case regular-case)))]

        ;; they're subtypes. easy.
        [(a b) #:when (cond
                        [(Type? a) (subtype a b obj)]
                        [(Result? a) (subresult a b)]
                        [else (subval a b)])
         empty]

        ;; Lists delegate to sequences
        [((ListSeq: s-seq) (ListSeq: t-seq))
         (cgen/seq context s-seq t-seq)]

        ;; refinements are erased to their bound
        [((Refinement: S _) T)
         (cg S T obj)]

        ;; variables that are in X and should be constrained
        ;; all other variables are compatible only with themselves
        [((F: (? (inferable-var? context) v)) T)
         #:return-when
         (match T
           ;; fail when v* is an index variable
           [(F: v*) (and (bound-index? v*) (not (bound-tvar? v*)))]
           [_ #f])
         #f
         ;; constrain v to be below T (but don't mention bounds)
         (singleton -Bottom v (var-demote T (context-bounds context)))]

        [(S (F: (? (inferable-var? context) v)))
         #:return-when
         (match S
           [(F: v*) (and (bound-index? v*) (not (bound-tvar? v*)))]
           [_ #f])
         #f
         ;; constrain v to be above S (but don't mention bounds)
         (singleton (var-promote S (context-bounds context)) v Univ)]

        ;; recursive names should get resolved as they're seen
        [(s (? Name? t))
         (let ([t (resolve-once t)])
           (and t (cg s t obj)))]
        [((? Name? s) t)
         (let ([s (resolve-once s)])
           (and s (cg s t obj)))]

        ;; constrain b1 to be below T, but don't mention the new vars
        [((Poly: v1 b1) T) (cgen (context-add context #:bounds v1) b1 T)]

        ;; Mu's just get unfolded
        [((? Mu? s) t) (cg (unfold s) t obj)]
        [(s (? Mu? t)) (cg s (unfold t) obj)]

        ;; find *an* element of elems which can be made a subtype of T
        [((Intersection: ts raw-prop) T)
         (let-values ([(obj new-props)
                       (cond
                         [(TrueProp? raw-prop) (values obj '())]
                         [(Object? obj) (values obj (list (instantiate-obj raw-prop obj)))]
                         [else (define new-obj (-id-path (genid)))
                               (values new-obj (list (instantiate-obj raw-prop new-obj)))])])
           (with-naively-extended-lexical-env [#:props new-props]
             (cset-join
              (for*/list ([t (in-list ts)]
                          [v (in-value (cg t T obj))]
                          #:when v)
                v))))]

        ;; constrain S to be below *each* element of elems, and then combine the constraints
        [(S (Intersection: ts raw-prop))
         (define cs (for/list/fail ([t (in-list ts)]) (cg S t obj)))
         (let ([obj (if (Object? obj) obj (-id-path (genid)))])
           (and cs
                (implies-in-env? (lexical-env)
                                 (-is-type obj S)
                                 (instantiate-obj raw-prop obj))
                (cset-meet* (cons empty cs))))]

        ;; constrain *each* element of es to be below T, and then combine the constraints
        [((BaseUnion-bases: es) T)
         (define cs (for/list/fail ([e (in-list es)]) (cg e T obj)))
         (and cs (cset-meet* (cons empty cs)))]
        [((Union-all: es) T)
         (define cs (for/list/fail ([e (in-list es)]) (cg e T obj)))
         (and cs (cset-meet* (cons empty cs)))]

        [(_ (Bottom:)) no-cset]

        ;; from define-new-subtype
        ;; NOTE: these cases for `((Distinction: _ _ _) _)`
        ;;  need to appear before the cases for `(_ (Union: _ _))`.
        ;;  See `typed-racket-test/succeed/in-hash-in-vector-subtype.rkt`
        [((Distinction: nm1 id1 S) (app resolve (Distinction: nm2 id2 T)))
         #:when (and (equal? nm1 nm2) (equal? id1 id2))
         (cg S T obj)]
        [((Distinction: _ _ S) T)
         (cg S T obj)]

        ;; find *an* element of es which can be made to be a supertype of S
        ;; FIXME: we're using multiple csets here, but I don't think it makes a difference
        ;; not using multiple csets will break for: ???
        [(S (Union-all: es))
         (cset-join
          (for*/list ([e (in-list es)]
                      [v (in-value (cg S e obj))]
                      #:when v)
            v))]

        ;; two structs with the same name
        ;; just check pairwise on the fields
        [((Struct: nm _ flds proc _ _ _) (Struct: nm* _ flds* proc* _ _ _))
         #:when (free-identifier=? nm nm*)
         (let ([proc-c
                (cond [(and proc proc*)
                       (cg proc proc*)]
                      [proc* #f]
                      [else empty])])
           (% cset-meet proc-c (cgen/flds context flds flds*)))]

        ;; two prefab structs with the same key
        [((Prefab: k ss) (Prefab: k* ts))
         #:when (and (prefab-key-subtype? k k*)
                     (>= (length ss) (length ts)))
         (% cset-meet*
            (for/list/fail ([s (in-list ss)]
                            [t (in-list ts)]
                            [mut? (in-list (prefab-key->field-mutability k*))])
                           (if mut?
                               (cgen/inv context s t)
                               (cgen context s t))))]

        ;; two struct names, need to resolve b/c one could be a parent
        [((Name: n _ #t) (Name: n* _ #t))
         (if (free-identifier=? n n*)
             empty ;; just succeed now
             (let ([S (resolve-once S)]
                   [T (resolve-once T)])
               (and S T (cg S T obj))))]
        ;; pairs are pointwise
        [((Pair: a b) (Pair: a* b*))
         (% cset-meet
            (cg a a* (-car-of obj))
            (cg b b* (-cdr-of obj)))]
        ;; sequences are covariant
        [((SequenceSeq: ts) (SequenceSeq: ts*))
         (cgen/seq context ts ts*)]
        [((Listof: t) (SequenceSeq: ts*))
         (cgen/seq context (seq (list t) -null-end) ts*)]
        [((Pair: t1 t2) (SequenceSeq: ts*))
         (% cset-meet
            (cgen/seq context (seq (list t1) -null-end) ts* (list (-car-of obj)))
            (cg t2 (-lst Univ) (-cdr-of obj))
            (cg t2 T (-cdr-of obj)))]
        [((MListof: t) (SequenceSeq: ts*))
         (cgen/seq context (seq (list t) -null-end) ts*)]
        ;; To check that mutable pair is a sequence we check that the cdr is
        ;; both an mutable list and a sequence
        [((MPair: t1 t2) (SequenceSeq: ts*))
         (% cset-meet
            (cgen/seq context (seq (list t1) -null-end) ts*)
            (cg t2 T)
            (cg t2 (Un -Null -MPairTop)))]
        [((List: ts) (SequenceSeq: ts*))
         (% cset-meet* (for/list/fail ([t (in-list ts)])
                         (cgen/seq context (seq (list t) -null-end) ts*)))]
        [((Immutable-HeterogeneousVector: ts) (Immutable-HeterogeneousVector: ts*))
         (cgen/list context ts ts*)]
        [((Mutable-HeterogeneousVector: ts) (Mutable-HeterogeneousVector: ts*))
         (% cset-meet (cgen/list context ts ts*) (cgen/list context ts* ts))]
        [((Immutable-HeterogeneousVector: ts) (Immutable-Vector: s))
         (define ss (map (λ _ s) ts))
         (cgen/list context ts ss)]
        [((Mutable-HeterogeneousVector: ts) (Mutable-Vector: s))
         (define ss (map (λ _ s) ts)) ;; invariant, everything has to match
         (% cset-meet (cgen/list context ts ss) (cgen/list context ss ts))]
        [((HeterogeneousVector: ts) (SequenceSeq: ts*))
         (% cset-meet* (for/list/fail ([t (in-list ts)])
                         (cgen/seq context (seq (list t) -null-end) ts*)))]
        [((Vector: t) (SequenceSeq: ts*))
         (cgen/seq context (seq (list t) -null-end) ts*)]
        [((? Base:String?) (SequenceSeq: ts*))
         (cgen/seq context (seq (list -Char) -null-end) ts*)]
        [((? Base:Bytes?) (SequenceSeq: ts*))
         (cgen/seq context (seq (list -Nat) -null-end) ts*)]
        [((? Base:Input-Port?) (SequenceSeq: ts*))
         (cgen/seq context (seq (list -Nat) -null-end) ts*)]
        [((Value: (? exact-nonnegative-integer? n)) (SequenceSeq: ts*))
         (define possibilities
           (list
            (list byte? -Byte)
            (list portable-index? -Index)
            (list portable-fixnum? -NonNegFixnum)
            (list values -Nat)))
         (define type
           (for/or ([pred-type (in-list possibilities)])
             (match pred-type
               [(list pred? type)
                (and (pred? n) type)])))
         (cgen/seq context (seq (list type) -null-end) ts*)]
        ;; numeric? == #true
        [((Base-bits: #t _) (SequenceSeq: ts*))
         (define type
           (for/or ([t (in-list (list -Byte -Index -NonNegFixnum -Nat))])
             (and (subtype S t) t)))
         (cgen/seq context (seq (list type) -null-end) ts*)]
        [((or (Mutable-HashTable: k v)
              (Immutable-HashTable: k v)
              (Weak-HashTable: k v))
          (SequenceSeq: ts*))
         (cgen/seq context (seq (list k v) -null-end) ts*)]
        [((Set: t) (SequenceSeq: ts*))
         (cgen/seq context (seq (list t) -null-end) ts*)]


        ;; resolve applications
        [((App: _ _) _)
         (let ([S (resolve-once S)])
           (and S (cg S T obj)))]
        [(_ (App: _ _))
         (let ([T (resolve-once T)])
           (and T (cg S T obj)))]

        ;; If the struct names don't match, try the parent of S
        ;; Needs to be done after App and Mu in case T is actually the current struct
        ;; but not currently visible
        [((Struct: nm (? Type? parent) _ _ _ _ _) other)
         (cg parent other)]

        ;; Invariant here because struct types aren't subtypes just because the
        ;; structs are (since you can make a constructor from the type).
        [((StructType: s) (StructType: t))
         (cg/inv s t)]

        ;; mutable vectors are invariant - generate constraints *both* ways
        [((Mutable-Vector: e) (Mutable-Vector: e*))
         (cg/inv e e*)]
        ;; immutable vectors are covariant
        [((Immutable-Vector: e) (Immutable-Vector: e*))
         (cg e e*)]
        ;; boxes are invariant - generate constraints *both* ways
        [((Box: e) (Box: e*))
         (cg/inv e e*)]
        [((Weak-Box: e) (Weak-Box: e*))
         (cg/inv e e*)]
        [((MPair: s t) (MPair: s* t*))
         (% cset-meet (cg/inv s s*) (cg/inv t t*))]
        [((Channel: e) (Channel: e*))
         (cg/inv e e*)]
        [((Async-Channel: e) (Async-Channel: e*))
         (cg/inv e e*)]
        [((ThreadCell: e) (ThreadCell: e*))
         (cg/inv e e*)]
        [((Continuation-Mark-Keyof: e) (Continuation-Mark-Keyof: e*))
         (cg/inv e e*)]
        [((Prompt-Tagof: s t) (Prompt-Tagof: s* t*))
         (% cset-meet (cg/inv s s*) (cg/inv t t*))]
        [((Promise: e) (Promise: e*))
         (cg e e*)]
        [((Ephemeron: e) (Ephemeron: e*))
         (cg e e*)]
        [((CustodianBox: e) (CustodianBox: e*))
         (cg e e*)]
        [((Set: a) (Set: a*))
         (cg a a*)]
        [((Evt: a) (Evt: a*))
         (cg a a*)]
        [((? Base:Semaphore?) (Evt: t))
         (cg S t)]
        [((? Base:Output-Port?) (Evt: t))
         (cg S t)]
        [((? Base:Input-Port?) (Evt: t))
         (cg S t)]
        [((? Base:TCP-Listener?) (Evt: t))
         (cg S t)]
        [((? Base:Thread?) (Evt: t))
         (cg S t)]
        [((? Base:Subprocess?) (Evt: t))
         (cg S t)]
        [((? Base:Will-Executor?) (Evt: t))
         (cg S t)]
        [((? Base:Log-Receiver?) (Evt: t ))
         (cg (make-Immutable-HeterogeneousVector
              (list -Symbol -String Univ
                    (Un (-val #f) -Symbol)))
             t)]
        [((? Base:Place?) (Evt: t))
         (cg Univ t)]
        [((? Base:Base-Place-Channel?) (Evt: t))
         (cg Univ t)]
        [((CustodianBox: t) (Evt: t*)) (cg S t*)]
        [((Channel: t) (Evt: t*)) (cg t t*)]
        [((Async-Channel: t) (Evt: t*)) (cg t t*)]
        [((Immutable-HashTable: s1 s2)
          (Immutable-HashTable: t1 t2))
         ;; for immutable hash tables, covariant
         (% cset-meet (cg s1 t1) (cg s2 t2))]
        [((Struct-Property: t1 _) (Struct-Property: t2 _))
         (cg t2 t1)]
        [((Mutable-HashTable: s1 s2)
          (Mutable-HashTable: t1 t2))
         ;; for mutable hash tables, invariant
         (% cset-meet (cg/inv s1 t1) (cg/inv s2 t2))]
        [((Weak-HashTable: s1 s2)
          (Weak-HashTable: t1 t2))
         ;; for mutable hash tables, invariant
         (% cset-meet (cg/inv s1 t1) (cg/inv s2 t2))]
        ;; syntax is covariant
        [((Syntax: s1) (Syntax: s2))
         (cg s1 s2)]
        ;; futures are covariant
        [((Future: s1) (Future: s2))
         (cg s1 s2)]
        ;; parameters are just like one-arg functions
        [((Param: in1 out1) (Param: in2 out2))
         (% cset-meet (cg in2 in1) (cg out1 out2))]
        [((Fun: s-arr)
          (Fun: t-arr))
         (% cset-meet*
            (for/list/fail
             ([t-arr (in-list t-arr)])
             ;; for each element of t-arr, we need to get at least one element of s-arr that works
             (let ([results (for*/list ([s-arr (in-list s-arr)]
                                        [v (in-value (cgen/arrow context s-arr t-arr))]
                                        #:when v)
                              v)])
               ;; ensure that something produces a constraint set
               (and (not (null? results))
                    (cset-join results)))))]
        [(_ _)
         ;; nothing worked, and we fail
         #f]))]))

;; C : set of constraints found by the inference engine
;; X : type variables that must have entries
;; Y : index variables that must have entries
;; R : result type into which we will be substituting
;; multiple-substitutions? : should we return one substitution (#f), or
;; all the substitutions that were possible? (#t)
;; NOTE: multiple substitutions are rare -- at the time of adding this
;; parameter this feature is only used by the tc-app/list.
;; NOTE: if multiple substitutions is #t, a list is returned,
;; otherwise a single substitution (not in a list) is returned.
(define/cond-contract (substs-gen C X Y R multiple-substitutions?)
  (cset? (listof symbol?) (listof symbol?) (or/c Values/c AnyValues? ValuesDots?) boolean?
         . -> . (or/c substitution/c
                      (cons/c substitution/c
                              (listof substitution/c))))
  (define var-hash (free-vars-hash (free-vars* R)))
  (define idx-hash (free-vars-hash (free-idxs* R)))
  ;; c : Constaint
  ;; variance : Variance
  (define (constraint->type v variance)
    (match v
      [(c S T)
       (match variance
         [(? variance:const?) S]
         [(? variance:co?) S]
         [(? variance:contra?) T]
         [(? variance:inv?) (let ([gS (generalize S)])
                             (if (subtype gS T)
                                 gS
                                 S))])]))

  ;; Since we don't add entries to the empty cset for index variables (since there is no
  ;; widest constraint, due to dcon-exacts), we must add substitutions here if no constraint
  ;; was found.  If we're at this point and had no other constraints, then adding the
  ;; equivalent of the constraint (dcon null (c Bot X Top)) is okay.
  (define (extend-idxs S)
    (hash-union
     (for/hash ([v (in-list Y)]
                #:unless (hash-has-key? S v))
       (let ([var (hash-ref idx-hash v variance:const)])
         (values v
                 (match var
                   [(? variance:const?) (i-subst null)]
                   [(? variance:co?) (i-subst null)]
                   [(? variance:contra?) (i-subst/starred null Univ)]
                   ;; TODO figure out if there is a better subst here
                   [(? variance:inv) (i-subst null)]))))
     S))
  (define (build-subst m)
    (match m
      [(cons cmap (dmap dm))
       (let* ([subst (hash-union
                      (for/hash ([(k dc) (in-hash dm)])
                        (define (c->t c) (constraint->type c (hash-ref idx-hash k variance:const)))
                        (values
                         k
                         (match dc
                           [(dcon fixed #f)
                            (i-subst (map c->t fixed))]
                           [(or (dcon fixed rest) (dcon-exact fixed rest))
                            (i-subst/starred
                             (map c->t fixed)
                             (c->t rest))]
                           [(dcon-dotted fixed dc dbound)
                            (i-subst/dotted
                             (map c->t fixed)
                             (c->t dc)
                             dbound)])))
                      (for/hash ([(k v) (in-hash cmap)])
                        (values k (t-subst (constraint->type v (hash-ref var-hash k variance:const))))))]
              [subst (for/fold ([subst subst]) ([v (in-list X)])
                       (let ([entry (hash-ref subst v #f)])
                         ;; Make sure we got a subst entry for a type var
                         ;; (i.e. just a type to substitute)
                         ;; If we don't have one, there are no constraints on this variable
                         (if (and entry (t-subst? entry))
                             subst
                             (hash-set subst v (t-subst Univ)))))])
         ;; verify that we got all the important variables
         (extend-idxs subst))]))
  (if multiple-substitutions?
      (for/list ([md (in-stream (cset-maps C))])
        (build-subst md))
      (build-subst (stream-first (cset-maps C)))))

;; context : the context of what to infer/not infer
;; S : a list of types to be the subtypes of T
;; T : a list of types
;; expected-cset : a cset representing the expected type, to meet early and
;;  keep the number of constraints in check. (empty by default)
;; produces a cset which determines a substitution that makes the Ss subtypes of the Ts
(define/cond-contract (cgen/list context S T [objs '()]
                                 #:expected-cset [expected-cset (empty-cset '() '())])
  (->* (context? (listof Values/c) (listof Values/c))
       ((listof (or/c #f OptObject?))
        #:expected-cset cset?)
       (or/c cset? #f))
  (and (= (length S) (length T))
       (% cset-meet*
          (for/list/fail ([s (in-list S)]
                          [t (in-list T)]
                          [obj (in-list/rest objs #f)])
                         ;; We meet early to prune the csets to a reasonable size.
                         ;; This weakens the inference a bit, but sometimes avoids
                         ;; constraint explosion.
            (% cset-meet (cgen context s t obj) expected-cset)))))



;; X : variables to infer
;; Y : indices to infer
;; S : actual argument types
;; T : formal argument types
;; R : result type
;; expected : #f or the expected type
;; returns a substitution
;; if R is #f, we don't care about the substituion
;; just return a boolean result
(define infer
 (let ()
   (define/cond-contract (infer X Y S T R [expected #f]
                                #:multiple? [multiple-substitutions? #f]
                                #:objs [objs '()])
     (((listof symbol?) (listof symbol?) (listof Type?) (listof Type?)
       (or/c #f Values/c AnyValues? ValuesDots?))
      ((or/c #f Values/c AnyValues? ValuesDots?)
       #:multiple? boolean?
       #:objs (listof OptObject?))
      . ->* . (or/c boolean?
                    substitution/c
                    (cons/c substitution/c
                            (listof substitution/c))))
     (define ctx (context null X Y))
     (define expected-cset
       (if expected
           (cgen ctx R expected)
           (empty-cset '() '())))
     (and expected-cset
          (let* ([cs (cgen/list ctx S T objs
                                #:expected-cset expected-cset)]
                 [cs* (% cset-meet cs expected-cset)])
            (and cs* (cond
                       [R (substs-gen cs* X Y R multiple-substitutions?)]
                       [else #t])))))
  ;(trace infer)
  infer)) ;to export a variable binding and not syntax

;; like infer, but T-var is the vararg type:
(define (infer/vararg X Y S T T-var R [expected #f]
                      #:objs [objs '()])
  (and ((length S) . >= . (length T))
       (let* ([fewer-ts (- (length S) (length T))]
              [new-T (match T-var
                       [(? Type? var-t) (list-extend S T var-t)]
                       [(Rest: rst-ts)
                        #:when (zero? (remainder fewer-ts (length rst-ts)))
                        (append T (repeat-list rst-ts
                                               (quotient fewer-ts (length rst-ts))))]
                       [_ T])])
         (infer X Y S new-T R expected #:objs objs))))

;; like infer, but dotted-var is the bound on the ...
;; and T-dotted is the repeated type
(define (infer/dots X dotted-var S T T-dotted R must-vars
                    #:expected [expected #f]
                    #:objs [objs (map (λ (_) #f) S)])
  (early-return
   (define-values (short-S rest-S) (split-at S (length T)))
   (define-values (short-objs rest-objs) (split-at objs (length T)))
   ;; Generate a new type corresponding to T-dotted for every extra arg.
   (define-values (new-vars new-Ts)
     (generate-dbound-prefix dotted-var T-dotted (length rest-S) #f))
   (define (subst t)
     (substitute-dots (map make-F new-vars) #f dotted-var t))
   (define ctx (context null (append new-vars X) (list dotted-var)))

   (define expected-cset (if expected
                             (cgen ctx (subst R) expected)
                             (empty-cset '() '())))
   #:return-unless expected-cset #f
   (define cs (% move-vars-to-dmap
                 (% cset-meet
                    (cgen/list ctx short-S (map subst T)
                               short-objs
                               #:expected-cset expected-cset)
                    (cgen/list ctx rest-S new-Ts
                               rest-objs
                               #:expected-cset expected-cset))
                 dotted-var new-vars))
   #:return-unless cs #f
   (define m (cset-meet cs expected-cset))
   #:return-unless m #f
   (substs-gen m X (list dotted-var) R #f)))


;(trace substs-gen)
;(trace cgen)
;(trace cgen/list)
;(trace cgen/arrow)
;(trace cgen/seq)
