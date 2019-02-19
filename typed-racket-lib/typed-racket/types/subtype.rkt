#lang racket/base
(require (except-in "../utils/utils.rkt" infer)
         racket/match racket/function racket/lazy-require
         racket/list
         (contract-req)
         (rep type-rep prop-rep object-rep
              core-rep type-mask values-rep rep-utils
              free-variance rep-switch)
         (utils tc-utils prefab identifier)
         (only-in (env type-env-structs)
                  with-lexical-env
                  with-naively-extended-lexical-env
                  lexical-env)
         (types utils resolve match-expanders current-seen
                numeric-tower substitute signatures)
         (for-syntax racket/base syntax/parse racket/sequence)
         "../infer/fail.rkt"
         (except-in (rename-in "abbrev.rkt"
                               [-> t->]
                               [->* t->*])
                    one-of/c))

(lazy-require
 ("../infer/infer.rkt" (infer))
 ("prop-ops.rkt" (-and))
 ("../typecheck/tc-subst.rkt" (instantiate-obj+simplify))
 ("../typecheck/tc-envops.rkt" (env+ implies-in-env?)))


(provide NameStruct:)

(provide/cond-contract
 [subtype (->* (Type? Type?) ((or/c #f OptObject?)) boolean?)]
 [subresult (-> Result? Result? boolean?)]
 [subval (-> SomeValues? SomeValues? boolean?)]
 [type-equiv? (-> Type? Type? boolean?)]
 [subtypes (-> (listof Type?) (listof Type?) boolean?)]
 [subtypes/varargs (-> (listof Type?)
                       (listof Type?)
                       (or/c Type? Rest? #f)
                       boolean?)]
 [unrelated-structs (-> Struct? Struct? boolean?)])


;; When subtype is called w/ no object, we
;; us a temporary object to name the arguments.
;; This parameter gives us plenty of fresh, temporary names
;; to use and this way we don't have to be constantly allocating
;; fresh identifiers.
(define temp-ids
  (make-parameter (make-id-seq)))
(define temp-objs
  (make-parameter (make-obj-seq)))

;;************************************************************
;; Public Interface to Subtyping
;;************************************************************

;; is t1 a subtype of t2?
;; if obj, then we're assuming obj is the subject
;; type type -> boolean
(define (subtype t1 t2 [obj #f])
  (and (subtype* (seen) t1 t2 obj) #t))


;; is v1 a subval of v2?
;; SomeValue? SomeValue -> boolean
;; (i.e. subtyping on SomeValues)
(define (subval v1 v2)
  (and (subval* (seen) v1 v2) #t))

;; are t1 and t2 equivalent types (w.r.t. subtyping)
(define (type-equiv? t1 t2)
  (and (type≡? (seen) t1 t2) #t))

;; are all the s's subtypes of all the t's?
;; [type] [type] -> boolean
(define (subtypes t1s t2s) (and (subtypes* (seen) t1s t2s) #t))

(define (subresult r1 r2) (and (subresult* (seen) r1 r2) #t))

;;************************************************************
;; General Subtyping Helpers
;;************************************************************


;; check subtyping for two lists of types, possibly terminated with a `Rest` or `RestDots`
;; List[(cons Number Number)] listof[type] listof[type] -> Opt[List[(cons Number Number)]]
(define/cond-contract (subtypes* A t1s t2s)
  (-> list? (listof Type?) (listof Type?) (or/c #f list?))
  (match* (t1s t2s)
    [((cons t1 rst1) (cons t2 rst2))
     (subtype-seq A
                  (subtype* t1 t2)
                  (subtypes* rst1 rst2))]
    [(_ _) (and (equal? t1s t2s) A)]))

(define (subresults* A rs1 rs2)
  (cond [(and (null? rs1) (null? rs2) A)]
        [(or (null? rs1) (null? rs2)) #f]
        [(subresult* A (car rs1) (car rs2))
         =>
         (λ (A*) (subresults* A* (cdr rs1) (cdr rs2)))]
        [else #f]))

(define (subvals* A vs1 vs2)
  (cond [(and (null? vs1) (null? vs2)) A]
        [(or (null? vs1) (null? vs2)) #f]
        [(subval* A (car vs1) (car vs2))
         =>
         (λ (A*) (subvals* A* (cdr vs1) (cdr vs2)))]
        [else #f]))

(define-syntax (let*/and stx)
  (syntax-parse stx
    [(_ () . e) (syntax/loc stx (let () . e))]
    [(_ ([id expr] . rest) . body)
     (syntax/loc stx
       (let ([id expr])
         (and id (let*/and rest . body))))]))

;; do notation for the subtyping monad
(define-syntax (subtype-seq stx)
  (define-syntax-class sub*
    (pattern e:expr))
  (syntax-parse stx
    [(_ init (s:sub* args:expr ... (~optional (~seq #:unless unless:expr)
                                              #:defaults ([unless #'#f])))
        ...+)
     (with-syntax ([(A* ... A-last) (generate-temporaries #'(s ...))])
       (with-syntax ([(clauses ...)
                      (for/list ([s (in-syntax #'(s ...))]
                                 [args (in-syntax #'((args ...) ...))]
                                 [skip-tst (in-syntax #'(unless ...))]
                                 [A (in-syntax #'(init A* ...))]
                                 [A-next (in-syntax #'(A* ... A-last))])
                        (cond
                          [(equal? (syntax-e #'tst) #f)
                           #`[#,A-next (#,s #,A . #,args)]]
                          [else
                           #`[#,A-next (or (and #,skip-tst #,A)
                                           (#,s #,A . #,args))]]))])
         (syntax/loc stx (let*/and (clauses ...)
                                   A-last))))]))


;;************************************************************
;; Function Subtyping Helpers
;;************************************************************


;; kw-subtypes : (Listof (Pairof Num Num)) (Listof Keyword) (Listof Keyword)
;;               -> (Option (Listof (Pairof Num Num)))
;;
;; Given function types F_s and F_t, this procedure is called to check that the
;; keyword types s-kws for F_s are subtypes of the keyword types t-kws for F_t
;; when checking that F_s <: F_t (but *not* F_t <: F_s).
;;
;; Note that in terms of width, s-kws may have more keywords (i.e., F_s accepts
;; all keywords that F_t does) but the types in s-kws must be supertypes of those
;; in t-kws (i.e., F_s domain types are at least as permissive as those of F_t).
(define (kw-subtypes* A kws1 kws2)
  (let loop ([A A] [kws1 kws1] [kws2 kws2])
    (and
     A
     (match* (kws1 kws2)
       [((cons (Keyword: k1 t1 r1) rest1) (cons (Keyword: k2 t2 r2) rest2))
        (cond [(eq? k2 k1)
               (and ;; if t is optional, s must be as well
                (or r2 (not r1))
                (loop (subtype* A t2 t1) rest1 rest2))]
              ;; optional extra keywords in s are ok
              ;; we just ignore them
              [(and (not r1) (keyword<? k1 k2)) (loop A rest1 kws2)]
              ;; extra keywords in t are a problem
              [else #f])]
       ;; no more keywords to satisfy, the rest in t must be optional
       [(_ '()) (and (andmap (match-lambda [(Keyword: _ _ r1) (not r1)])
                             kws1) A)]
       ;; we failed to satisfy all the keyword
       [(_ _) #f]))))


;; Based soley on the domain, is one arrow (i.e. dom1 + rst1)
;; a subtype of another arrow (i.e. dom1 + rst1)?
;; NOTE: This function takes into account that domains are
;; contravariant w.r.t. subtyping, i.e. callers should NOT
;; flip argument order.
(define/cond-contract (Arrow-domain-subtypes* A dom1 rst1 dom2 rst2 [objs #f])
  (->* (list?
        (listof Type?)
        (or/c #f Rest? RestDots?)
        (listof Type?)
        (or/c #f Rest? RestDots?))
       ((listof Object?))
       (or/c #f list?))
  (match* (dom1 dom2)
    [((cons t1 ts1) (cons t2 ts2))
     (subtype-seq A
                  (subtype* t2 t1 (and objs (car objs)))
                  (Arrow-domain-subtypes* ts1 rst1 ts2 rst2 (and objs (cdr objs))))]
    [(_ _)
     (subtype* A
               (-Tuple* dom2 (Rest->Type rst2))
               (-Tuple* dom1 (Rest->Type rst1)))]))

(define-syntax-rule (with-fresh-ids len ids . body)
  (let-values ([(ids seq) (for/fold ([ids '()]
                                     [seq (temp-ids)])
                                    ([_ (in-range len)])
                            (define-values (id rst) (id-seq-next seq))
                            (values (cons id ids) rst))])
    (parameterize ([temp-ids seq])
      . body)))

;; simple co/contra-variance for ->
(define/cond-contract (arrow-subtype* A arr1 arr2)
  (-> list? Arrow? Arrow? (or/c #f list?))
  (match* (arr1 arr2)
    [((Arrow: dom1 rst1 kws1 raw-rng1)
      (Arrow: dom2 rst2 kws2 raw-rng2))
     (define A* (subtype-seq A
                             (Arrow-domain-subtypes* dom1 rst1 dom2 rst2)
                             (kw-subtypes* kws1 kws2)))
     (cond
       [(not A*) #f]
       [else
        (with-fresh-ids (length dom2) ids
          (define mapping
            (for/list ([idx (in-naturals)]
                       [id (in-list ids)]
                       [t (in-list dom2)])
              (list* idx id t)))
          (subval* A*
                   (instantiate-obj+simplify raw-rng1 mapping)
                   (instantiate-obj raw-rng2 ids)))])]))


;; is an Arrow a subtype of a DepFun?
;; more or less the following:
;;        ⊢ T3 <: T1
;; x : T3 ⊢ T2 <: T4
;; -----------------------
;; ⊢ (T1 → T2) <: (x:T3)→T4
(define/cond-contract (arrow-subtype-dfun* A arrow dfun)
  (-> list? Arrow? DepFun? (or/c #f list?))
  (match* (arrow dfun)
    [((Arrow: dom1 rst1 kws1 raw-rng1)
      (DepFun: raw-dom2 raw-pre2  raw-rng2))
     #:when (Arrow-includes-arity? arrow (length raw-dom2))
     (define arity (length raw-dom2))
     (with-fresh-ids arity ids
       (define dom2 (for/list ([d (in-list raw-dom2)])
                      (instantiate-obj d ids)))
       (define A* (subtype-seq A
                               (kw-subtypes* kws1 '())
                               (Arrow-domain-subtypes* dom1 rst1 dom2 #f (map -id-path ids))))
       (cond
         [(not A*) #f]
         [else
          (define pre2 (instantiate-obj raw-pre2 ids))
          (define-values (mapping t2s)
            (for/lists (_1 _2)
              ([idx (in-range arity)]
               [id (in-list ids)]
               [t (in-list/rest dom2 Univ)])
              (values (list* idx id t) t)))
          (with-naively-extended-lexical-env
              [#:identifiers ids
               #:types t2s
               #:props (list pre2)]
            (subval* A*
                     (instantiate-obj+simplify raw-rng1 mapping)
                     (instantiate-obj raw-rng2 ids)))]))]
    [(_ _) #f]))

;;************************************************************
;; Prop 'Subtyping'
;;************************************************************

;; check subtyping of props, so that predicates subtype correctly
(define (prop-subtype* A p1 p2)
  (match* (p1 p2)
    [(p p) A]
    [((? FalseProp?) t) A]
    [(_ (? TrueProp?)) A]
    [((TypeProp: o1 t1)
      (TypeProp: o2 t2))
     #:when (equal? o1 o2)
     (subtype* A t1 t2)]
    [((NotTypeProp: o1 t1)
      (NotTypeProp: o2 t2))
     #:when (equal? o1 o2)
     (subtype* A t2 t1)]
    [(_ _) #f]))

(define (subtypes/varargs args dom rst)
  (and (subtypes*/varargs null args dom rst) #t))

; subtypes*/varargs : list?
;                     (listof Type)
;                     (listof Type)
;                     (or/c #f Type Rest)
;                     (or/c #f (listof Object))
; ->
; list? or #f
(define/cond-contract (subtypes*/varargs A argtys dom raw-rst)
  (-> list? (listof Type?) (listof Type?) (or/c #f Type? Rest? RestDots?)
      (or/c #f list?))
  (define rst (match raw-rst
                [(? Type?) (make-Rest (list raw-rst))]
                [_ raw-rst]))
  (Arrow-domain-subtypes* A dom rst argtys #f))


;;************************************************************
;; Struct Helpers
;;************************************************************


(define-match-expander NameStruct:
  (lambda (stx)
    (syntax-case stx ()
      [(_ i)
       #'(or (and (Name/struct:)
                  (app resolve-once (? Struct? i)))
             (App: (and (Name/struct:)
                        (app resolve-once (Poly: _ (? Struct? i))))
                   _))])))

(define (unrelated-structs s1 s2)
  (define (in-hierarchy? s par)
    (define s-name
      (match s
        [(Poly: _ (Struct: s-name _ _ _ _ _ _)) s-name]
        [(Struct: s-name _ _ _ _ _ _) s-name]))
    (define p-name
      (match par
        [(Poly: _ (Struct: p-name _ _ _ _ _ _)) p-name]
        [(Struct: p-name _ _ _ _ _ _) p-name]))
    (or (free-identifier=? s-name p-name)
        (match s
          [(Poly: _ (? Struct? s*)) (in-hierarchy? s* par)]
          [(Struct: _ (and (Name/struct:) p) _ _ _ _ _)
           (in-hierarchy? (resolve-once p) par)]
          [(Struct: _ (? Struct? p) _ _ _ _ _) (in-hierarchy? p par)]
          [(Struct: _ (Poly: _ p) _ _ _ _ _) (in-hierarchy? p par)]
          [(Struct: _ #f _ _ _ _ _) #f]
          [_ (int-err "what is this?!?! ~a" s)])))
  (not (or (in-hierarchy? s1 s2) (in-hierarchy? s2 s1))))


;;************************************************************
;; Values Subtyping
;;************************************************************


(define/cond-contract (subval* A v1 v2)
  (-> (listof (cons/c Type? Type?)) SomeValues? SomeValues?
      any/c)
  (match* (v1 v2)
    ;; subtyping on values is pointwise, except special case for Bottom
    [((or (Values: (list (Result: (== -Bottom) _ _)))
          (Values: (list (Result: _ (PropSet: (? FalseProp?) (? FalseProp?)) _))))
      _)
     A]
    [((Values: results1) (Values: results2))
     (subresults* A results1 results2)]
    [((ValuesDots: rs1 dty1 dbound)
      (ValuesDots: rs2 dty2 dbound))
     (subtype-seq A
                  (subresults* rs1 rs2)
                  (subtype* dty1 dty2))]
    [((AnyValues: prop1) (AnyValues: prop2))
     (prop-subtype* A prop1 prop2)]
    [((or (Values: (list (Result: _ ps1 _) ...))
          (ValuesDots: (list (Result: _ ps1 _) ...) _ _))
      (AnyValues: prop2))
     (ormap (match-lambda
              [(PropSet: p1+ p1-)
               (subtype-seq A
                            (prop-subtype* p1+ prop2)
                            (prop-subtype* p1- prop2))])
            ps1)]
    [(_ _) #f]))

;;************************************************************
;; Result Subtyping
;;************************************************************

(define/cond-contract (subresult* A res1 res2)
  (-> (listof (cons/c Type? Type?)) Result? Result?
      any/c)
  (match* (res1 res2)
    [((Result: t1 (PropSet: p1+ p1-) o1)
      (Result: t2 (PropSet: p2+ p2-) o2))
     (and (or (equal? o1 o2) (Empty? o2) (not o2))
          (subtype-seq A
                       (subtype* t1 t2 o1)
                       (prop-subtype* p1+ p2+)
                       (prop-subtype* p1- p2-)))]))

;;************************************************************
;; Type Subtyping
;;************************************************************

(define/cond-contract (type≡? A t1 t2)
  (-> list? Type? Type? any/c)
  (subtype-seq A
               (subtype* t1 t2)
               (subtype* t2 t1)))

(define-syntax-rule (with-fresh-obj obj . body)
  (let-values ([(obj seq) (obj-seq-next (temp-objs))])
    (parameterize ([temp-objs seq])
      . body)))


;; the algorithm for subtyping for recursive types,
;; initially transcribed directly from TAPL, pg 305
;; and since then heavily added to
;;
;; List[(cons Number Number)] Type Type (Object or #f)
;; -> List[(cons Number Number)] or #f
;;
;; is t1 a subtype of t2, taking into account previously seen pairs A,
;; and given that we are inquiring about Object 'obj' (relevant when
;; the types include refinements which may be proven by propositions
;; in the current lexical environment)
;;
;; <><NOTE><> the seen list (A) should be updated for the following
;; types as they are encountered:
;; needs-resolved? types (Mus, Names, Apps),
;; Instances, and Structs (Prefabs?)
(define (subtype* A t1 t2 [obj #f])
  (cond
    [(Univ? t2) A]
    [(Bottom? t1) A]
    ;; error is top and bot
    [(or (Error? t1) (Error? t2)) A]
    [(disjoint-masks? (mask t1) (mask t2)) #f]
    [(equal? t1 t2) A]
    [(seen? t1 t2 A) A]
    [else
     ;; first we check on a few t2 cases
     ;; that need to come early during checking
     (match* (obj t2)
       [(_ (Intersection: t2s raw-prop))
        (let ([A (for/fold ([A A])
                           ([t2 (in-list t2s)]
                            #:break (not A))
                   (subtype* A t1 t2 obj))])
          (and A
               (or (TrueProp? raw-prop)
                   (let ([A (remember t1 t2 A)])
                     (with-updated-seen A
                       (let* ([obj (if (Object? obj) obj (-id-path (genid)))]
                              [prop (instantiate-obj raw-prop obj)])
                         (implies-in-env? (lexical-env)
                                          (-is-type obj t1)
                                          prop)))))
               A))]
       [(_ (? resolvable?))
        (let ([A (remember t1 t2 A)])
          (with-updated-seen A
            (let ([t2 (resolve-once t2)])
              ;; check needed for if a name that hasn't been resolved yet
              (and (Type? t2) (subtype* A t1 t2 obj)))))]
       [((? Object? obj) (app int-type->provable-range
                              (cons lower-bound upper-bound)))
        #:when (and (with-refinements?)
                    (subtype* A t1 -Int obj)
                    (let ([A (remember t1 t2 A)])
                      (with-updated-seen A
                        (provable-int-subtype? A t1 t2 lower-bound upper-bound obj))))
        A]
       [(_ _) ;; otherwise we case on t1
        (subtype-cases A t1 t2 obj)])]))

;; if obj ∈ t1, can we prove 'lower-bound <= obj' and 'obj <= upper-bound'?
(define (provable-int-subtype? A t1 t2 lower-bound upper-bound obj)
  (define lower-ineq
    (cond
      [lower-bound (-leq (-lexp lower-bound)
                         (-lexp obj))]
      [else -tt]))
  (define upper-ineq
    (cond
      [upper-bound (-leq (-lexp obj)
                         (-lexp upper-bound))]
      [else -tt]))
  (cond
    ;; at least one inequality was a contradiction, fail!
    [(or (FalseProp? lower-ineq) (FalseProp? upper-ineq)) #f]
    ;; both inequalities were trivially true, succeed!
    [(and (TrueProp? lower-ineq) (TrueProp? upper-ineq)) A]
    [else
     (let ([A (remember t1 t2 A)])
       (with-updated-seen A
         ;; be provable for subtyping to hold
         (define-values (t1* extracted-props) (extract-props obj t1))
         (define assumptions (apply -and (cons (-is-type obj t1*) extracted-props)))

         (define goal
           (match* (lower-ineq upper-ineq)
             [((? TrueProp?) p) p]
             [(p (? TrueProp?)) p]
             [(_ _) (make-AndProp (list lower-ineq upper-ineq))]))
         (implies-in-env? (lexical-env)
                          assumptions
                          goal)))]))


(define (continue<: A t1 t2 obj)
  (match* (t1 t2)
    [(t1 (Union/set: base2 ts2 elems2))
     (cond
       [(hash-has-key? elems2 t1) A]
       [(subtype* A t1 base2 obj)]
       [else (for/or ([elem2 (in-list ts2)])
               (subtype* A t1 elem2 obj))])]
    [(_ (Instance: (? resolvable? t2*)))
     (let ([A (remember t1 t2 A)])
       (with-updated-seen A
         (let ([t2* (resolve-once t2*)])
           (and (Type? t2*)
                (subtype* A t1 (make-Instance t2*) obj)))))]
    [(_ (Poly: vs2 b2))
     #:when (null? (fv b2))
     (subtype* A t1 b2 obj)]
    [(_ (PolyDots: vs2 b2))
     #:when (and (null? (fv b2))
                 (null? (fi b2)))
     (subtype* A t1 b2 obj)]
    [(_ _) #f]))




;; is this a sequence of arrows of the form
;; τ0 -> σ ∧ τ1 -> σ ∧ τn -> σ ...
;; if so, return
;; (∪ τ0 τ1 ... τn) -> σ
;; else return #f
(define/cond-contract (collapsable-arrows? arrows)
  (-> (listof Arrow?) (or/c Arrow? #f))
  (match arrows
    [(cons (Arrow: (list dom1) #f '() rng) remaining)
     (match remaining
       [(list (Arrow: (list dom2) #f '() (== rng))
              (Arrow: (list doms) #f '() (== rng)) ...)
        (-Arrow (list (apply Un dom1 dom2 doms)) rng)]
       [_ #f])]
    [_ #f]))


;; these data structures are allocated once and
;; used below in 'subtype-switch'
(define seq->elem-table
  (hash -FlVector    -Flonum
        -ExtFlVector -ExtFlonum
        -FxVector    -Fixnum
        -String      -Char
        -Bytes       -Byte
        -Input-Port  -Nat))

(define event-types
  (list -Semaphore
        -Output-Port
        -Input-Port
        -TCP-Listener
        -Thread
        -Subprocess
        -Will-Executor))

(define event-univ-types (list -Place -Base-Place-Channel))
(define num-seq-types (list -Byte -Index -NonNegFixnum -Nat))
(define log-vect-type (make-HeterogeneousVector
                       (list -Symbol -String Univ
                             (Un -False -Symbol))))
(define null-or-mpair-top (Un -Null -MPairTop))

(define value-numeric-seq-possibilities
  (list
   (cons byte? -Byte)
   (cons portable-index? -Index)
   (cons portable-fixnum? -NonNegFixnum)
   (cons values -Nat)))


(define-rep-switch (subtype-cases A (#:switch t1) t2 obj)
  ;; NOTE: keep these in alphabetical order
  ;; for ease of finding cases
  [(case: App _)
   (let ([A (remember t1 t2 A)])
     (with-updated-seen A
       (let ([t1 (resolve-once t1)])
         ;; check needed for if a name that hasn't been resolved yet
         (and (Type? t1) (subtype* A t1 t2)))))]
  [(case: Async-Channel (Async-Channel: elem1))
   (match t2
     [(? Async-ChannelTop?) A]
     [(Async-Channel: elem2) (type≡? A elem1 elem2)]
     [(Evt: evt-t) (subtype* A elem1 evt-t)]
     [_ (continue<: A t1 t2 obj)])]
  [(case: Base (Base-bits: num? bits))
   (match t2
     [(BaseUnion: bbits nbits)
      (and (if num?
               (nbits-overlap? nbits bits)
               (bbits-overlap? bbits bits))
           A)]
     [(or (SequenceTop:) (Sequence: (list _)))
      (define seq-t (if (SequenceTop? t2) Univ (first (Sequence-tys t2))))
      (cond
        [(Base:Null? t1) A]
        [(hash-ref seq->elem-table t1 #f)
         => (λ (elem-ty) (subtype* A elem-ty seq-t))]
        [num?
         (define type
           ;; FIXME: thread the store through here
           (for/or ([num-t (in-list num-seq-types)])
             (or (and (subtype* A t1 num-t) num-t))))
         (if type
             (subtype* A type seq-t)
             #f)]
        [else #f])]
     [(Evt: evt-t)
      (cond
        [(member t1 event-types)
         (subtype* A t1 evt-t)]
        ;; FIXME: change Univ to Place-Message-Allowed if/when that type is defined
        [(and (Univ? evt-t) (member t1 event-univ-types))
         A]
        [(Base:Log-Receiver? t1)
         (subtype* A log-vect-type evt-t)]
        [else #f])]
     [_ (continue<: A t1 t2 obj)])]
  [(case: BaseUnion (BaseUnion: bbits1 nbits1))
   (match t2
     [(? Base?) #f]
     [(BaseUnion: bbits2 nbits2)
      (and (bbits-subset? bbits1 bbits2)
           (nbits-subset? nbits1 nbits2)
           A)]
     [(Union: (BaseUnion: bbits2 nbits2) _)
      #:when (and (bbits-subset? bbits1 bbits2)
                  (nbits-subset? nbits1 nbits2))
      A]
     [_ (for/fold ([A A])
                  ([b (in-list (BaseUnion-bases t1))]
                   #:break (not A))
          (subtype* A b t2 obj))])]
  [(case: Box (Box: elem1))
   (match t2
     [(? BoxTop?) A]
     [(Box: elem2) (type≡? A elem1 elem2)]
     [_ (continue<: A t1 t2 obj)])]
  [(case: Channel (Channel: elem1))
   (match t2
     [(? ChannelTop?) A]
     [(Channel: elem2) (type≡? A elem1 elem2)]
     [(Evt: evt-t) (subtype* A elem1 evt-t)]
     [_ (continue<: A t1 t2 obj)])]
  [(case: Class (Class: row inits fields methods augments init-rest))
   (match t2
     [(? ClassTop?) A]
     [(Class: row* inits* fields* methods* augments* init-rest*)
      ;; TODO: should the result be folded instead?
      (define (sub t1 t2) (subtype* A t1 t2))
      ;; check that each of inits, fields, methods, etc. are
      ;; equal by sorting and checking type equality
      (define (equal-clause? clause clause* [inits? #f])
        (cond
          [(not inits?)
           (match-define (list (list names types) ...) clause)
           (match-define (list (list names* types*) ...) clause*)
           (and (= (length names) (length names*))
                (andmap equal? names names*)
                (andmap sub types types*))]
          [else
           (match-define (list (list names types opt?) ...)
             clause)
           (match-define (list (list names* types* opt?*) ...)
             clause*)
           (and (= (length names) (length names*))
                (andmap equal? names names*)
                (andmap sub types types*)
                (andmap equal? opt? opt?*))]))
      ;; There is no non-trivial width subtyping on class types, but it's
      ;; possible for two "equal" class types to look different
      ;; in the representation. We deal with that here.
      (and (or (and (or (Row? row) (not row))
                    (or (Row? row*) (not row*)))
               (equal? row row*))
           (equal-clause? inits inits* #t)
           (equal-clause? fields fields*)
           (equal-clause? methods methods*)
           (equal-clause? augments augments*)
           (or (and init-rest init-rest*
                    (sub init-rest init-rest*))
               (and (not init-rest) (not init-rest*)
                    A)))]
     [_ (continue<: A t1 t2 obj)])]
  [(case: Continuation-Mark-Keyof (Continuation-Mark-Keyof: val1))
   (match t2
     [(? Continuation-Mark-KeyTop?) A]
     [(Continuation-Mark-Keyof: val2)
      (type≡? A val1 val2)]
     [_ (continue<: A t1 t2 obj)])]
  [(case: CustodianBox (CustodianBox: elem1))
   (match t2
     [(CustodianBox: elem2) (subtype* A elem1 elem2)]
     [(Evt: evt-t)
      ;; Note that it's the whole box type that's being
      ;; compared against evt-t here
      (subtype* A t1 evt-t)]
     [_ (continue<: A t1 t2 obj)])]
  [(case: DepFun (DepFun: raw-dom1 raw-pre1 raw-rng1))
   (match t2
     [(DepFun: raw-dom2 raw-pre2 raw-rng2)
      (cond
        [(not (= (length raw-dom1)
                 (length raw-dom2)))
         #f]
        [else
         (with-fresh-ids (length raw-dom1) ids
           (define dom1 (for/list ([d (in-list raw-dom1)])
                          (instantiate-obj d ids)))
           (define pre1 (instantiate-obj raw-pre1 ids))
           (define rng1 (instantiate-obj raw-rng1 ids))
           (define dom2 (for/list ([d (in-list raw-dom2)])
                          (instantiate-obj d ids)))
           (define pre2 (instantiate-obj raw-pre2 ids))
           (define rng2 (instantiate-obj raw-rng2 ids))
           (with-naively-extended-lexical-env
               [#:identifiers ids
                #:types dom2
                #:props (list pre2)]
             (define A* (for/fold ([A (subval* A rng1 rng2)])
                                  ([d1 (in-list dom1)]
                                   [d2 (in-list dom2)]
                                   [id (in-list ids)]
                                   #:break (not A))
                          (subtype* A d2 d1 (-id-path id))))
             (and (implies-in-env? (lexical-env) pre2 pre1)
                  A*)))])]
     [(Fun: arrows2)
      (define arity (for/fold ([arity (length raw-dom1)])
                              ([a2 (in-list arrows2)])
                      (max arity (length (Arrow-dom a2)))))
      (with-fresh-ids arity ids
        (define dom1 (for/list ([d (in-list raw-dom1)])
                       (instantiate-obj d ids)))
        (define pre1 (instantiate-obj raw-pre1 ids))
        (for/fold ([A A])
                  ([a2 (in-list arrows2)]
                   #:break (not A))
          (match a2
            [(Arrow: dom2 rst2 kws2 raw-rng2)
             (define A* (subtype-seq A
                                     (subtypes* dom2 dom1)
                                     (kw-subtypes* '() kws2)))
             (cond
               [(not A*) #f]
               [else
                (define arity (max (length dom1) (length dom2)))
                (define-values (mapping t2s)
                  (for/lists (_1 _2)
                    ([idx (in-range arity)]
                     [id (in-list ids)])
                    (define t (dom+rst-ref dom2 rst2 idx Univ))
                    (values (list* idx id t) t)))
                (with-naively-extended-lexical-env
                    [#:identifiers ids
                     #:types t2s]
                  (define A-res
                    (subval* A*
                             (instantiate-obj+simplify raw-rng1 mapping)
                             (instantiate-obj raw-rng2 ids)))
                  (and (implies-in-env? (lexical-env) -tt pre1)
                       A-res))])])))]
     [_ (continue<: A t1 t2 obj)])]
  [(case: Distinction (Distinction: nm1 id1 t1*))
   (match t2
     [(app resolve (Distinction: nm2 id2 t2*))
      #:when (and (equal? nm1 nm2) (equal? id1 id2))
      (subtype* A t1* t2*)]
     [_ (cond
          [(subtype* A t1* t2 obj)]
          [else (continue<: A t1 t2 obj)])])]
  [(case: Ephemeron (Ephemeron: elem1))
   (match t2
     [(Ephemeron: elem2) (subtype* A elem1 elem2)]
     [_ (continue<: A t1 t2 obj)])]
  [(case: Evt (Evt: result1))
   (match t2
     [(Evt: result2) (subtype* A result1 result2)]
     [_ (continue<: A t1 t2 obj)])]
  [(case: F (F: var1))
   (match t2
     ;; tvars are equal if they are the same variable
     [(F: var2) (eq? var1 var2)]
     [_ (continue<: A t1 t2 obj)])]
  [(case: Fun (Fun: arrows1))
   (match* (t2 arrows1)
     ;; special case when t1 can be collapsed into simpler arrow
     [((Fun: (list arrow2)) (app collapsable-arrows? (? Arrow? arrow1)))
      (arrow-subtype* A arrow1 arrow2)]
     ;; special case when t1 can be collapsed into simpler arrow
     [((? DepFun? dfun) (app collapsable-arrows? (? Arrow? arrow1)))
      (arrow-subtype-dfun* A arrow1 dfun)]
     [((Fun: arrows2) _)
      (cond
        [(null? arrows1) #f]
        [else (for/fold ([A A])
                        ([a2 (in-list arrows2)]
                         #:break (not A))
                (for/or ([a1 (in-list arrows1)])
                  (arrow-subtype* A a1 a2)))])]
     [((? DepFun? dfun) _)
      (for/or ([a1 (in-list arrows1)])
        (arrow-subtype-dfun* A a1 dfun))]
     [(_ _) (continue<: A t1 t2 obj)])]
  [(case: Future (Future: elem1))
   (match t2
     [(Future: elem2) (subtype* A elem1 elem2)]
     [_ (continue<: A t1 t2 obj)])]
  [(case: Immutable-HashTable (Immutable-HashTable: key1 val1))
   (match t2
    [(Immutable-HashTable: key2 val2)
     (subtype-seq A
                  (subtype* key1 key2)
                  (subtype* val1 val2))]
    [(SequenceTop:) A]
    [(Sequence: (list key2 val2))
     (subtype-seq A
                  (subtype* key1 key2)
                  (subtype* val1 val2))]
    [(or (Mutable-HashTableTop:) (Mutable-HashTable: _ _)
         (Weak-HashTableTop:) (Weak-HashTable: _ _))
     #false]
    [_ (continue<: A t1 t2 obj)])]
  [(case: Immutable-HeterogeneousVector (Immutable-HeterogeneousVector: elems1))
   (match t2
     [(Immutable-HeterogeneousVector: elems2)
      (and (= (length elems1)
              (length elems2))
           (for/fold ([A A])
                     ([elem1 (in-list elems1)]
                      [elem2 (in-list elems2)]
                      #:break (not A))
             (subtype* A elem1 elem2)))]
     [(Immutable-Vector: elem2)
      (for/fold ([A A])
                ([elem1 (in-list elems1)] #:break (not A))
        (subtype* A elem1 elem2))]
     [(or (? Mutable-VectorTop?)
          (? Mutable-Vector?))
      #false]
     [(SequenceTop:) A]
     [(Sequence: (list seq-t))
      (for/fold ([A A])
                ([elem1 (in-list elems1)]
                 #:break (not A))
        (subtype* A elem1 seq-t))]
     [_ (continue<: A t1 t2 obj)])]
  [(case: Immutable-Vector (Immutable-Vector: elem1))
   (match t2
     [(Immutable-Vector: elem2)
      (subtype* A elem1 elem2)]
     [(or (? Mutable-VectorTop?)
          (? Mutable-Vector?))
      #false]
     [(SequenceTop:) A]
     [(Sequence: (list seq-t))
      (subtype* A elem1 seq-t)]
     [_ (continue<: A t1 t2 obj)])]
  [(case: Instance (Instance: inst-t1))
   (cond
     [(resolvable? inst-t1)
      (let ([A (remember t1 t2 A)])
        (with-updated-seen A
          (let ([t1* (resolve-once inst-t1)])
            (and (Type? t1*)
                 (subtype* A (make-Instance t1*) t2)))))]
     [else
      (match* (t1 t2)
        [((Instance: (Class: _ _ field-map method-map augment-map _))
          (Instance: (Class: _ _ field-map* method-map* augment-map* _)))
         (define (subtype-clause? map map*)
           (and (for/and ([key+type (in-list map*)])
                  (match-define (list key type) key+type)
                  (assq key map))
                (let/ec escape
                  (for/fold ([A A])
                            ([key+type (in-list map)]
                             #:break (not A))
                    (match-define (list key type) key+type)
                    (define result (assq (car key+type) map*))
                    (or (and (not result) A)
                        (let ([type* (cadr result)])
                          (or (subtype* A type type*)
                              (escape #f))))))))
         (and ;; Note that init & augment clauses don't matter for objects
          (subtype-clause? method-map method-map*)
          (subtype-clause? field-map field-map*))]
        [(_ _) (continue<: A t1 t2 obj)])])]
  [(case: Intersection (Intersection: t1s _))
   (match t1
     [(Refine: t1* raw-prop)
      (parameterize ([with-refinements? #t])
        (cond
          [(Object? obj)
           (define prop (instantiate-obj raw-prop obj))
           (define env (env+ (lexical-env) (list prop)))
           (cond
             [(not env) A]
             [else (with-lexical-env env
                     (subtype* A t1* t2 obj))])]
          [else
           (with-fresh-obj obj
             (define prop (instantiate-obj raw-prop obj))
             ;; since this is a fresh object, we will do a simpler environment extension
             (with-naively-extended-lexical-env [#:props (list prop)]
               (subtype* A t1* t2 obj)))]))]
     [_
      (cond
        [(for/or ([t1 (in-list t1s)])
           (subtype* A t1 t2 obj))]
        [else (continue<: A t1 t2 obj)])])]
  [(case: ListDots (ListDots: dty1 dbound1))
   (match t2
     ;; recur structurally on dotted lists, assuming same bounds
     [(ListDots: dty2 dbound2)
      (and (eq? dbound1 dbound2)
           (subtype* A dty1 dty2))]
     ;; For dotted lists and regular lists, we check that (All
     ;; (dbound1) dty1) is a subtype of elem2, so that no matter
     ;; what dbound is instatiated with dty1 is still a subtype of
     ;; elem2. We cannot just replace dbound with Univ because of
     ;; variance issues.
     [(Listof: elem2)
      (subtype* A (-poly (dbound1) dty1) elem2)]
     [_ (continue<: A t1 t2 obj)])]
  [(case: MPair (MPair: t11 t12))
   (match t2
     [(? MPairTop?) A]
     [(MPair: t21 t22)
      (subtype-seq A
                   (type≡? t11 t21)
                   (type≡? t12 t22))]
     ;; To check that mutable pair is a sequence we check that the cdr
     ;; is both an mutable list and a sequence
     [(SequenceTop:)
      (subtype-seq A
                   (subtype* t12 null-or-mpair-top)
                   (subtype* t12 -SequenceTop))]
     [(Sequence: (list seq-t))
      (subtype-seq A
                   (subtype* t11 seq-t)
                   (subtype* t12 null-or-mpair-top)
                   (subtype* t12 (make-Sequence (list seq-t))))]
     [_ (continue<: A t1 t2 obj)])]
  [(case: Mu _)
   (let ([A (remember t1 t2 A)])
     (with-updated-seen A
       (let ([t1 (unfold t1)])
         ;; check needed for if a name that hasn't been resolved yet
         (and (Type? t1) (subtype* A t1 t2)))))]
  [(case: Mutable-HashTable (Mutable-HashTable: key1 val1))
   (match t2
    [(Mutable-HashTableTop:) A]
    [(Mutable-HashTable: key2 val2)
     (subtype-seq A
                  (type≡? key1 key2)
                  (type≡? val1 val2))]
    [(SequenceTop:) A]
    [(Sequence: (list key2 val2))
     (subtype-seq A
                  (subtype* key1 key2)
                  (subtype* val1 val2))]
    [(or (Weak-HashTableTop:) (Weak-HashTable: _ _) (Immutable-HashTable: _ _))
     #false]
    [_ (continue<: A t1 t2 obj)])]
  [(case: Mutable-HeterogeneousVector (Mutable-HeterogeneousVector: elems1))
   (match t2
     [(Immutable-Vector: elem2)
      #false]
     [(Mutable-HeterogeneousVector: elems2)
      (and (= (length elems1)
              (length elems2))
           (for/fold ([A A])
                     ([elem1 (in-list elems1)]
                      [elem2 (in-list elems2)]
                      #:break (not A))
             (type≡? A elem1 elem2)))]
     [(Mutable-VectorTop:)
      A]
     [(Mutable-Vector: elem2)
      (for/fold ([A A])
                ([elem1 (in-list elems1)] #:break (not A))
        (type≡? A elem1 elem2))]
     [(SequenceTop:) A]
     [(Sequence: (list seq-t))
      (for/fold ([A A])
                ([elem1 (in-list elems1)]
                 #:break (not A))
        (subtype* A elem1 seq-t))]
     [_ (continue<: A t1 t2 obj)])]
  [(case: Mutable-Vector (Mutable-Vector: elem1))
   (match t2
     [(Mutable-VectorTop:)
      A]
     [(Mutable-Vector: elem2)
      (type≡? A elem1 elem2)]
     [(SequenceTop:) A]
     [(Sequence: (list seq-t))
      (subtype* A elem1 seq-t)]
     [(Immutable-Vector: elem2)
      #false]
     [_ (continue<: A t1 t2 obj)])]
  [(case: Name _)
   (match* (t1 t2)
     ;; Avoid resolving things that refer to different structs.
     ;; Saves us from non-termination
     [((NameStruct: s1) (or (? Struct? s2) (NameStruct: s2)))
      #:when (unrelated-structs s1 s2)
      #f]
     [(_ _)
      (let ([A (remember t1 t2 A)])
        (with-updated-seen A
          (let ([t1 (resolve-once t1)])
            ;; check needed for if a name that hasn't been resolved yet
            (and (Type? t1) (subtype* A t1 t2)))))])]
  [(case: Pair (Pair: t11 t12))
   (match t2
     [(Pair: t21 t22)
      (subtype-seq A
                   (subtype* t11 t21 (-car-of obj))
                   (subtype* t12 t22 (-cdr-of obj)))]
     [(SequenceTop:)
      (subtype* A t12 (-lst Univ))]
     [(Sequence: (list seq-t))
      (subtype-seq A
                   (subtype* t11 seq-t)
                   (subtype* t12 (-lst seq-t)))]
     [_ (continue<: A t1 t2 obj)])]
  [(case: Param (Param: in1 out1))
   (match t2
     [(Param: in2 out2) (subtype-seq A
                                     (subtype* in2 in1)
                                     (subtype* out1 out2))]
     [_ (subtype* A (cl->* (t-> out1) (t-> in1 -Void)) t2)]
     [_ (continue<: A t1 t2 obj)])]
  [(case: Poly (Poly: names b1))
   (match t2
     [(? Poly?) #:when (= (length names) (Poly-n t2))
                (subtype* A b1 (Poly-body names t2))]
     ;; use local inference to see if we can use the polytype here
     [_ #:when (infer names null (list b1) (list t2) Univ) A]
     [_ (continue<: A t1 t2 obj)])]
  [(case: PolyDots (PolyDots: (list ns ... n-dotted) b1))
   (match t2
     [(PolyDots: (list ms ... m-dotted) b2)
      (cond
        [(< (length ns) (length ms))
         (define-values (short-ms rest-ms) (split-at ms (length ns)))
         ;; substitute ms for ns in b1 to make it look like b2
         (define subst
           (hash-set (make-simple-substitution ns (map make-F short-ms))
                     n-dotted (i-subst/dotted (map make-F rest-ms) (make-F m-dotted) m-dotted)))
         (subtype* A (subst-all subst b1) b2)]
        [else
         (define-values (short-ns rest-ns) (split-at ns (length ms)))
         ;; substitute ns for ms in b2 to make it look like b1
         (define subst
           (hash-set (make-simple-substitution ms (map make-F short-ns))
                     m-dotted (i-subst/dotted (map make-F rest-ns) (make-F n-dotted) n-dotted)))
         (subtype* A b1 (subst-all subst b2))])]
     [(Poly: ms b2)
      #:when (<= (length ns) (length ms))
      ;; substitute ms for ns in b1 to make it look like b2
      (define subst
        (hash-set (make-simple-substitution ns (map make-F (take ms (length ns))))
                  n-dotted (i-subst (map make-F (drop ms (length ns))))))
      (subtype* A (subst-all subst b1) b2)]
     [_ #:when (infer ns (list n-dotted) (list b1) (list t2) Univ)
        A]
     [_ (continue<: A t1 t2 obj)])]
  [(case: Prefab (Prefab: k1 ss))
   (match t2
     [(Prefab: k2 ts)
      (let ([A (remember t1 t2 A)])
        (with-updated-seen A
          (and (prefab-key-subtype? k1 k2)
               (and (>= (length ss) (length ts))
                    (for/fold ([A A])
                              ([s (in-list ss)]
                               [t (in-list ts)]
                               [mut? (in-list (prefab-key->field-mutability k2))]
                               #:break (not A))
                      (and A
                           (if mut?
                               (subtype-seq A
                                            (subtype* t s)
                                            (subtype* s t))
                               (subtype* A s t))))))))]
     [(PrefabTop: k2) (and (prefab-key-subtype? k1 k2) A)]
     [_ (continue<: A t1 t2 obj)])]
  [(case: PrefabTop (PrefabTop: k1))
   (match t2
     [(Prefab: k2 flds)
      (and (prefab-key-subtype? k1 k2)
           (not (prefab-key/mutable-fields? k2))
           (for/fold ([A A])
                     ([fld-t (in-list flds)]
                      ;; only check the fields both have in common
                      [_ (in-range (prefab-key->field-count k2))]
                      #:break (not A))
             (subtype* A Univ fld-t)))]
     [(PrefabTop: k2) (and (prefab-key-subtype? k1 k2) A)]
     [_ (continue<: A t1 t2 obj)])]
  [(case: Promise (Promise: elem1))
   (match t2
     [(Promise: elem2) (subtype* A elem1 elem2)]
     [_ (continue<: A t1 t2 obj)])]
  [(case: Prompt-Tagof (Prompt-Tagof: body1 handler1))
   (match t2
     [(? Prompt-TagTop?) A]
     [(Prompt-Tagof: body2 handler2)
      (subtype-seq A
                   (type≡? body1 body2)
                   (type≡? handler1 handler2))]
     [_ (continue<: A t1 t2 obj)])]
  [(case: Refinement (Refinement: t1-parent id1))
   (match t2
     [(Refinement: t2-parent id2)
      #:when (free-identifier=? id1 id2)
      (subtype* A t1-parent t2-parent)]
     [_ (cond
          [(subtype* A t1-parent t2)]
          [else (continue<: A t1 t2 obj)])])]
  ;; sequences are covariant
  [(case: Sequence (Sequence: ts1))
   (match t2
     [(SequenceTop:) A]
     [(Sequence: ts2) (subtypes* A ts1 ts2)]
     [_ (continue<: A t1 t2 obj)])]
  [(case: Set (Set: elem1))
   (match t2
     [(Set: elem2) (subtype* A elem1 elem2)]
     [(SequenceTop:) A]
     [(Sequence: (list seq-t)) (subtype* A elem1 seq-t)]
     [_ (continue<: A t1 t2 obj)])]
  [(case: Struct (Struct: nm1 parent1 flds1 proc1 _ _ _))
   (match t2
     ;; Avoid resolving things that refer to different structs.
     ;; Saves us from non-termination
     [(or (? Struct? t2) (NameStruct: t2))
      #:when (unrelated-structs t1 t2)
      #f]
     ;; subtyping on immutable structs is covariant
     [(Struct: nm2 _ flds2 proc2 _ _ _)
      #:when (free-identifier=? nm1 nm2)
      (let ([A (remember t1 t2 A)])
        (with-updated-seen A
          (let ([A (cond [(and proc1 proc2) (subtype* A proc1 proc2)]
                         [proc2 #f]
                         [else A])])
            (for/fold ([A A])
                      ([f1 (in-list flds1)]
                       [f2 (in-list flds2)]
                       [idx (in-naturals)]
                       #:break (not A))
              (match* (f1 f2)
                [((fld: fty1 _ mutable1?) (fld: fty2 _ mutable2?))
                 #:when (eq? mutable1? mutable2?)
                 (cond
                   [mutable1? (type≡? A fty1 fty2)]
                   [else (subtype* A fty1 fty2 (-struct-idx-of t1 idx obj))])]
                [(_ _) #f])))))]
     [(StructTop: (Struct: nm2 _ _ _ _ _ _))
      #:when (free-identifier=? nm1 nm2)
      A]
     [(Val-able: (? (negate struct?) _)) #f]
     ;; subtyping on structs follows the declared hierarchy
     [_ (cond
          [(and (Type? parent1)
                (let ([A (remember t1 t2 A)])
                  (with-updated-seen A
                    (subtype* A parent1 t2))))]
          [else (continue<: A t1 t2 obj)])]
     [_ (continue<: A t1 t2 obj)])]
  [(case: StructType (StructType: t1*))
   (match t2
     [(StructTypeTop:) A]
     [_ (continue<: A t1 t2 obj)])]
  [(case: Syntax (Syntax: elem1))
   (match t2
     [(Syntax: elem2) (subtype* A elem1 elem2)]
     [_ (continue<: A t1 t2 obj)])]
  [(case: ThreadCell (ThreadCell: elem1))
   (match t2
     [(? ThreadCellTop?) A]
     [(ThreadCell: elem2) (type≡? A elem1 elem2)]
     [_ (continue<: A t1 t2 obj)])]
  [(case: Union (Union/set: base1 ts1 elems1))
   (let ([A (subtype* A base1 t2 obj)])
     (and A
          (match t2
            [(Union/set: base2 ts2 elems2)
             (for/fold ([A A])
                       ([elem1 (in-list ts1)]
                        #:break (not A))
               (cond
                 [(hash-has-key? elems2 elem1) A]
                 [(subtype* A elem1 base2 obj)]
                 [else (subtype* A elem1 t2 obj)]))]
            [_ (for/fold ([A A])
                         ([elem1 (in-list ts1)]
                          #:break (not A))
                 (subtype* A elem1 t2 obj))])))]
  ;; For Unit types invoke-types are covariant
  ;; imports and init-depends are covariant in that importing fewer
  ;; signatures results in a subtype
  ;; exports conversely are contravariant, subtypes export more signatures
  [(case: Unit (Unit: imports1 exports1 init-depends1 t1*))
   (match t2
     [(? UnitTop?) A]
     [(Unit: imports2 exports2 init-depends2 t2*)
      (and (check-sub-signatures? imports2 imports1)
           (check-sub-signatures? exports1 exports2)
           (check-sub-signatures? init-depends2 init-depends1)
           (subval* A t1* t2*))]
     [_ (continue<: A t1 t2 obj)])]
  [(case: Value (Value: val1))
   (match t2
     [(Base-predicate: pred) (and (pred val1) A)]
     [(BaseUnion-bases: bs)
      (for*/or ([b (in-list bs)]
                [pred (in-value (Base-predicate b))])
        (and (pred val1) A))]
     [(SequenceTop:)
      (and (exact-nonnegative-integer? val1) A)]
     [(Sequence: (list seq-t))
      (cond
        [(exact-nonnegative-integer? val1)
         (define type
           (for*/or ([pred/type (in-list value-numeric-seq-possibilities)]
                     [pred? (in-value (car pred/type))]
                     #:when (pred? val1))
             (cdr pred/type)))
         (subtype* A type seq-t)]
        [else #f])]
     [(or (? Struct? s1) (NameStruct: s1))
      #:when (not (struct? val1))
      #f]
     [(Refine: t2* p*)
      #:when (and (exact-integer? val1)
                  (let ([obj (-lexp val1)])
                    (and (subtype* A t1 t2*)
                         (implies-in-env? (lexical-env)
                                          (-is-type obj t1)
                                          (instantiate-obj p* obj)))))
      A]
     [_ (continue<: A t1 t2 obj)])]
  [(case: Weak-Box (Weak-Box: elem1))
   (match t2
     [(? Weak-BoxTop?) A]
     [(Weak-Box: elem2) (type≡? A elem1 elem2)]
     [_ (continue<: A t1 t2 obj)])]
  [(case: Weak-HashTable (Weak-HashTable: key1 val1))
   (match t2
    [(Weak-HashTableTop:) A]
    [(Weak-HashTable: key2 val2)
     (subtype-seq A
                  (type≡? key1 key2)
                  (type≡? val1 val2))]
    [(SequenceTop:) A]
    [(Sequence: (list key2 val2))
     (subtype-seq A
                  (subtype* key1 key2)
                  (subtype* val1 val2))]
    [(or (Mutable-HashTableTop:) (Mutable-HashTable: _ _)
         (Immutable-HashTable: _ _))
     #false]
    [_ (continue<: A t1 t2 obj)])]
  [else: (continue<: A t1 t2 obj)])
