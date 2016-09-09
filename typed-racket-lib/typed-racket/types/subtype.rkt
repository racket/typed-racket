#lang racket/base
(require (except-in "../utils/utils.rkt" infer)
         racket/match racket/function racket/lazy-require
         racket/list
         (contract-req)
         (rep type-rep prop-rep object-rep
              core-rep type-mask values-rep rep-utils
              free-variance)
         (utils tc-utils early-return)
         (types utils resolve match-expanders current-seen
                numeric-tower substitute prefab signatures)
         (for-syntax racket/base syntax/parse racket/sequence)
         (rename-in "base-abbrev.rkt"
                    [-> t->]
                    [->* t->*]))

(lazy-require
  ("union.rkt" (Un))
  ("../infer/infer.rkt" (infer))
  ("../typecheck/tc-subst.rkt" (restrict-values)))


(provide/cond-contract
 [subtype (-> Type? Type? boolean?)]
 [subresult (-> Result? Result? boolean?)]
 [subval (-> SomeValues? SomeValues? boolean?)]
 [type-compare? (-> (or/c Type? SomeValues?) (or/c Type? SomeValues?) boolean?)]
 [subtypes (-> (listof Type?) (listof Type?) boolean?)]
 [subtypes/varargs (-> (listof Type?) (listof Type?) (or/c Type? #f) boolean?)])


;;************************************************************
;; Public Interface to Subtyping
;;************************************************************

;; is s a subtype of t?
;; type type -> boolean
(define (subtype t1 t2)
  (define res (and (subtype* (seen) t1 t2) #t))
  res)

(define (subval t1 t2)
  (and (subval* (seen) t1 t2) #t))

(define (type-compare? t1 t2)
  (or (equal? t1 t2) (and (subtype t1 t2)
                          (subtype t2 t1))))

;; are all the s's subtypes of all the t's?
;; [type] [type] -> boolean
(define (subtypes t1s t2s) (and (subtypes* (seen) t1s t2s) #t))

(define (subresult r1 r2) (and (subresult* (seen) r1 r2) #t))

;;************************************************************
;; General Subtyping Helpers
;;************************************************************


;; check subtyping for two lists of types
;; List[(cons Number Number)] listof[type] listof[type] -> Opt[List[(cons Number Number)]]
(define (subtypes* A t1s t2s)
  (cond [(and (null? t1s) (null? t2s) A)]
        [(or (null? t1s) (null? t2s)) #f]
        [(subtype* A (car t1s) (car t2s))
         =>
         (λ (A*) (subtypes* A* (cdr t1s) (cdr t2s)))]
        [else #f]))

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

;; check if s is a supertype of any element of ts
(define (supertype-of-one/arr A s ts)
  (for/or ([t (in-list ts)])
    (arr-subtype*/no-fail A t s)))

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



(define/cond-contract (combine-arrs arrs)
  (-> (listof arr?) (or/c #f arr?))
  (match arrs
    [(list (and a1 (arr: dom1 rng1 #f #f '())) (arr: dom rng #f #f '()) ...)
     (cond
       [(null? dom) (make-arr dom1 rng1 #f #f '())]
       [(not (apply = 1 (length dom1) (map length dom))) #f]
       [(not (for/and ([rng2 (in-list rng)]) (equal? rng1 rng2)))
        #f]
       [else (make-arr (apply map Un (cons dom1 dom)) rng1 #f #f '())])]
    [_ #f]))

;; simple co/contra-variance for ->
(define/cond-contract (arr-subtype*/no-fail A arr1 arr2)
  (-> list? arr? arr? any/c)
  (match* (arr1 arr2)
    ;; the really simple case
    [((arr: dom1 rng1 #f #f '())
      (arr: dom2 rng2 #f #f '()))
     (subtype-seq A
                  (subtypes* dom2 dom1)
                  (subval* (restrict-values rng1 dom2) rng2))]
    [((arr: dom1 rng1 #f #f kws1)
      (arr: dom2 rng2 #f #f kws2))
     (subtype-seq A
                  (subtypes* dom2 dom1)
                  (kw-subtypes* kws1 kws2)
                  (subval* (restrict-values rng1 dom2) rng2))]
    [((arr: dom1 rng1 rest1 #f kws1)
      (arr: dom2 rng2 #f    #f kws2))
     (subtype-seq A
                  (subtypes*/varargs dom2 dom1 rest1)
                  (kw-subtypes* kws1 kws2)
                  (subval* (restrict-values rng1 dom2) rng2))]
    [((arr: dom1 rng1 #f    #f kws1)
      (arr: dom2 rng2 rest2 #f kws2))
     #f]
    [((arr: dom1 rng1 rest1 #f kws1)
      (arr: dom2 rng2 rest2 #f kws2))
     (subtype-seq A
                  (subtypes*/varargs dom2 dom1 rest1)
                  (subtype* rest2 rest1)
                  (kw-subtypes* kws1 kws2)
                  (subval* (restrict-values rng1 dom2) rng2))]
    ;; handle ... varargs when the bounds are the same
    [((arr: dom1 rng1 #f (cons drest1 dbound) kws1)
      (arr: dom2 rng2 #f (cons drest2 dbound) kws2))
     (subtype-seq A
                  (subtype* drest2 drest1)
                  (subtypes* dom2 dom1)
                  (kw-subtypes* kws1 kws2)
                  (subval* (restrict-values rng1 dom2) rng2))]
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

(define (subtypes*/varargs A argtys dom rst)
  (let loop-varargs ([dom dom] [argtys argtys] [A A])
    (cond
      [(not A) #f]
      [(and (null? dom) (null? argtys)) A]
      [(null? argtys) #f]
      [(and (null? dom) rst)
       (cond [(subtype* A (car argtys) rst) => (λ (A) (loop-varargs dom (cdr argtys) A))]
             [else #f])]
      [(null? dom) #f]
      [(subtype* A (car argtys) (car dom)) => (λ (A) (loop-varargs (cdr dom) (cdr argtys) A))]
      [else #f])))


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
                   _ _))])))

(define (subtype/flds* A flds flds*)
  (for/fold ([A A])
            ([f (in-list flds)] [f* (in-list flds*)]
             #:break (not A))
    (match* (f f*)
      [((fld: t _ #t) (fld: t* _ #t))
       (subtype-seq A
                    (subtype* t* t)
                    (subtype* t t*))]
      [((fld: t _ #f) (fld: t* _ #f))
       (subtype* A t t*)]
      [(_ _) #f])))

(define (unrelated-structs s1 s2)
  (define (in-hierarchy? s par)
    (define s-name
      (match s
        [(Poly: _ (Struct: s-name _ _ _ _ _)) s-name]
        [(Struct: s-name _ _ _ _ _) s-name]))
    (define p-name
      (match par
        [(Poly: _ (Struct: p-name _ _ _ _ _)) p-name]
        [(Struct: p-name _ _ _ _ _) p-name]))
    (or (free-identifier=? s-name p-name)
        (match s
          [(Poly: _ (? Struct? s*)) (in-hierarchy? s* par)]
          [(Struct: _ (and (Name/struct:) p) _ _ _ _)
           (in-hierarchy? (resolve-once p) par)]
          [(Struct: _ (? Struct? p) _ _ _ _) (in-hierarchy? p par)]
          [(Struct: _ (Poly: _ p) _ _ _ _) (in-hierarchy? p par)]
          [(Struct: _ #f _ _ _ _) #f]
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
                       (subtype* t1 t2)
                       (prop-subtype* p1+ p2+)
                       (prop-subtype* p1- p2-)))]))

;;************************************************************
;; Type Subtyping
;;************************************************************

(define/cond-contract (type-equiv? A t1 t2)
  (-> list? Type? Type? any/c)
  (subtype-seq A
	       (subtype* t1 t2)
	       (subtype* t2 t1)))

;; the algorithm for recursive types transcribed directly from TAPL, pg 305
;; List[(cons Number Number)] type type -> List[(cons Number Number)] or #f
;; is s a subtype of t, taking into account previously seen pairs A
;;
;; <><NOTE><> the seen list (A) should be updated for the following
;; types as they are encountered:
;; needs-resolved? types (Mus, Names, Apps),
;; Instances, and Structs (Prefabs?)
(define/cond-contract (subtype* A t1 t2)
  (-> (listof (cons/c Type? Type?)) Type? Type? (or/c #f list?))
  (early-return
   #:return-when (seen? t1 t2 A) A
   #:return-when (Univ? t2) A
   ;; error is top and bot
   #:return-when (or (type-equal? t1 Err)
                     (type-equal? t2 Err)) A
   #:return-when (type-equal? t1 -Bottom) A
   (define mask1 (Type-mask t1))
   (define mask2 (Type-mask t2))
   #:return-when (disjoint-masks? mask1 mask2) #f
   #:return-when (type-equal? t1 t2) A
   (define t1-subtype-cache (Type-subtype-cache t1))
   (define cr (hash-ref t1-subtype-cache (Rep-seq t2) 'missing))
   #:return-when (boolean? cr) (and cr A)
   (define result
     (match* (t1 t2)
       [((Intersection: t1s) _)
        (for/or ([t1 (in-list t1s)])
          (subtype* A t1 t2))]
       [(_ (Intersection: t2s))
        (for/fold ([A A])
                  ([t2 (in-list t2s)]
                   #:break (not A))
          (subtype* A t1 t2))]
       ;; from define-new-subtype
       [((Distinction: nm1 id1 t1) (app resolve (Distinction: nm2 id2 t2)))
        #:when (and (equal? nm1 nm2) (equal? id1 id2))
        (subtype* A t1 t2)]
       [((Distinction: _ _ t1) t2) (subtype* A t1 t2)]
       ;; tvars are equal if they are the same variable
       [((F: var1) (F: var2)) (and (eq? var1 var2) A)]
       ;; structural types of the same kind can be checked by simply
       ;; referencing the field variances and performing the
       ;; appropriate recursive calls
       [((? structural? t1) (? structural? t2))
        #:when (eq? (Rep-name t1)
                    (Rep-name t2))
        (for/fold ([A A])
                  ([v (in-list (Type-variances t1))]
                   [t1 (in-list (Rep-values t1))]
                   [t2 (in-list (Rep-values t2))]
                   #:break (not A))
          (cond
            [(eq? v Covariant)
             (subtype* A t1 t2)]
            [(eq? v Invariant)
             (type-equiv? A t1 t2)]
            [else ;; Contravariant
             (subtype* A t2 t1)]))]
       ;; If the type has a registered top type predicate, let's check it!
       [((? has-top-type?) _) #:when ((top-type-pred t1) t2) A]
       ;; quantification over two types preserves subtyping
       [((Poly: ns b1) (Poly: ms b2))               
        #:when (= (length ns) (length ms))
        ;; substitute ns for ms in b2 to make it look like b1
        (subtype* A b1 (subst-all (make-simple-substitution ms (map make-F ns)) b2))]
       [((PolyDots: (list ns ... n-dotted) b1)
         (PolyDots: (list ms ... m-dotted) b2))
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
       [((PolyDots: (list ns ... n-dotted) b1)
         (Poly: (list ms ...) b2))
        #:when (<= (length ns) (length ms))
        ;; substitute ms for ns in b1 to make it look like b2
        (define subst
          (hash-set (make-simple-substitution ns (map make-F (take ms (length ns))))
                    n-dotted (i-subst (map make-F (drop ms (length ns))))))
        (subtype* A (subst-all subst b1) b2)]
       ;; use unification to see if we can use the polytype here
       [((Poly: vs1 b1) _)
        #:when (infer vs1 null (list b1) (list t2) Univ)
        A]
       [((PolyDots: (list vs1 ... vdotted1) b1) _)
        #:when (infer vs1 (list vdotted1) (list b1) (list t2) Univ)
        A]
       [(_ (or (Poly:     vs2 b2)
               (PolyDots: vs2 b2)))
        #:when (null? (fv b2))
        (subtype* A t1 b2)]
       ;; recur structurally on dotted lists, assuming same bounds
       [((ListDots: dty1 dbound1) (ListDots: dty2 dbound2))
        (and (eq? dbound1 dbound2)
             (subtype* A dty1 dty2))]
       ;; For dotted lists and regular lists, we check that (All
       ;; (dbound) s-dty) is a subtype of t-elem, so that no matter
       ;; what dbound is instatiated with s-dty is still a subtype of
       ;; t-elem. We cannot just replace dbound with Univ because of
       ;; variance issues.
       [((ListDots: dty1 dbound1) (Listof: t2-elem))
        (subtype* A (-poly (dbound1) dty1) t2-elem)]
       [((Value: v) (Base: _ _ pred _)) (if (pred v) A #f)]
       [((? needs-resolved?) _)
        (let ([A (remember t1 t2 A)])
          (with-updated-seen A
            (let ([t1 (resolve-once t1)])
              ;; check needed for if a name that hasn't been resolved yet
              (and (Type? t1) (subtype* A t1 t2)))))]
       [(_ (? needs-resolved?))
        (let ([A (remember t1 t2 A)])
          (with-updated-seen A
            (let ([t2 (resolve-once t2)])
              ;; check needed for if a name that hasn't been resolved yet
              (and (Type? t2) (subtype* A t1 t2)))))]
       [((Union: elems) t)
        (for/fold ([A A])
                  ([elem (in-list elems)]
                   #:break (not A))
          (subtype* A elem t))]
       [(s (Union: elems))
        (and (ormap (λ (elem) (subtype* A s elem)) elems) A)]
       ;; Avoid needing to resolve things that refer to different structs.
       ;; Saves us from non-termination
       ;; Must happen *before* the sequence cases, which sometimes call `resolve' in match expanders
       [((or (? Struct? s1) (NameStruct: s1))
         (or (? Struct? s2) (NameStruct: s2)))
        #:when (unrelated-structs s1 s2)
        #f]
       ;; same for all values.
       [((Value: (? (negate struct?) _)) (or (? Struct? s1) (NameStruct: s1)))
        #f]
       [((or (? Struct? s1) (NameStruct: s1)) (Value: (? (negate struct?) _)))
        #f]
       ;; sequences are covariant
       [((Sequence: ts1) (Sequence: ts2)) (subtypes* A ts1 ts2)]
       [((Hashtable: k1 v1) (Sequence: (list k2 v2)))
        (subtype-seq A
                     (subtype* k1 k2)
                     (subtype* v1 v2))]
       ;; special-case for case-lambda/union with only one argument              
       [((Function: arr1) (Function: (list arr2)))
        (cond [(null? arr1) #f]
              [else
               (define comb (combine-arrs arr1))
               (or (and comb (arr-subtype*/no-fail A comb arr2))
                   (supertype-of-one/arr A arr2 arr1))])]
       ;; case-lambda
       [((Function: arrs1) (Function: arrs2))
        (if (null? arrs1) #f
            (let loop-arities ([A A]
                               [arrs2 arrs2])
              (cond
                [(null? arrs2) A]
                [(supertype-of-one/arr A (car arrs2) arrs1)
                 => (λ (A) (loop-arities A (cdr arrs2)))]
                [else #f])))]
       [((Refinement: t1-parent _) _)
        (subtype* A t1-parent t2)]
       ;; subtyping on immutable structs is covariant
       [((Struct: nm1 _ flds1 proc1 _ _) (Struct: nm2 _ flds2 proc2 _ _)) 
        #:when (free-identifier=? nm1 nm2) 
        (let ([A (remember t1 t2 A)])
          (with-updated-seen A
            (let ([A (cond [(and proc1 proc2) (subtype* A proc1 proc2)]
                           [proc2 #f]
                           [else A])])
              (and A (subtype/flds* A flds1 flds2)))))]
       [((Struct: nm1 _ _ _ _ _) (StructTop: (Struct: nm2 _ _ _ _ _)))
        #:when (free-identifier=? nm1 nm2)
        A]
       ;; vector special cases
       [((HeterogeneousVector: elems1) (Vector: t2))
        (for/fold ([A A])
                  ([elem1 (in-list elems1)] #:break (not A))
          (type-equiv? A elem1 t2))]
       [((HeterogeneousVector: elems1) (HeterogeneousVector: elems2))
        (cond [(= (length elems1)
                  (length elems2))
               (for/fold ([A A])
                         ([elem1 (in-list elems1)]
                          [elem2 (in-list elems2)]
                          #:break (not A))
                 (type-equiv? A elem1 elem2))]
              [else #f])]
       ;; subtyping on structs follows the declared hierarchy
       [((Struct: nm1 (? Type? parent1) _ _ _ _) _)
        (let ([A (remember t1 t2 A)])
          (with-updated-seen A
            (subtype* A parent1 t2)))]
       [((Prefab: k1 ss) (Prefab: k2 ts))
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
       ;; subtyping on other stuff
       [((Param: in1 out1) _)
        (subtype* A (cl->* (t-> out1) (t-> in1 -Void)) t2)]
       ;; homogeneous Sequence call helper for remaining cases
       ;; that are subtypes of a homogeneous Sequence
       [(_ (Sequence: (list seq-t))) (homo-sequence-subtype A t1 seq-t)]
       ;; events call off to helper for remaining cases
       [(_ (Evt: evt-t)) (event-subtype A t1 evt-t)]
       [((Instance: (? needs-resolved? t1*)) _)
        (let ([A (remember t1 t2 A)])
          (with-updated-seen A
            (let ([t1* (resolve-once t1*)])
              (and (Type? t1*)
                   (subtype* A (make-Instance t1*) t2)))))]
       [(_ (Instance: (? needs-resolved? t2*)))
        (let ([A (remember t1 t2 A)])
          (with-updated-seen A
            (let ([t2* (resolve-once t2*)])
              (and (Type? t2*)
                   (subtype* A t1 (make-Instance t2*))))))]
       [((Instance: (Class: _ _ field-map method-map augment-map _))
         (Instance: (Class: _ _ field-map* method-map* augment-map* _)))
        (define (subtype-clause? map map*)
          (and (for/and ([key+type (in-list map*)])
                 (match-define (list key type) key+type)
                 (assq key map))
               (let/ec escape
                 (for/fold ([A A])
                           ([key+type (in-list map)])
                   (match-define (list key type) key+type)
                   (define result (assq (car key+type) map*))
                   (or (and (not result) A)
                       (let ([type* (cadr result)])
                         (or (subtype* A type type*)
                             (escape #f))))))))
        (and ;; Note that init & augment clauses don't matter for objects
         (subtype-clause? method-map method-map*)
         (subtype-clause? field-map field-map*))]
       [((Class: row inits fields methods augments init-rest)
         (Class: row* inits* fields* methods* augments* init-rest*))
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
       ;; For Unit types invoke-types are covariant
       ;; imports and init-depends are covariant in that importing fewer
       ;; signatures results in a subtype
       ;; exports conversely are contravariant, subtypes export more signatures
       [((Unit: imports1 exports1 init-depends1 t1)
         (Unit: imports2 exports2 init-depends2 t2))
        (and (check-sub-signatures? imports2 imports1)
             (check-sub-signatures? exports1 exports2)
             (check-sub-signatures? init-depends2 init-depends1)
             (subval* A t1 t2))]
       ;; otherwise, not a subtype
       [(_ _) #f]))
   (when (null? A) (hash-set! t1-subtype-cache (Rep-seq t2) (and result #t)))
   result))

;;************************************************************
;; Other Subtyping Special Cases
;;************************************************************

(define seq-base-types `((FlVector . ,-Flonum)
                         (ExtFlVector . ,-ExtFlonum)
                         (FxVector . ,-Fixnum)
                         (String . ,-Char)
                         (Bytes . ,-Byte)
                         (Input-Port . ,-Nat)))

;; Homo-sequence-subtype
;; is t a subtype of (Sequence: seq-t)?
(define/cond-contract (homo-sequence-subtype A t seq-t)
  (-> list? Type? Type? any/c)
  (match t
    [(Pair: t1 t2)
     (subtype-seq A
                  (subtype* t1 seq-t)
                  (subtype* t2 (-lst seq-t)))]
    ;; To check that mutable pair is a sequence we check that the cdr
    ;; is both an mutable list and a sequence
    [(MPair: t1 t2)
     (subtype-seq A
                  (subtype* t1 seq-t)
                  (subtype* t2 (simple-Un -Null (make-MPairTop)))
                  (subtype* t2 (make-Sequence (list seq-t))))]
    [(Value: '()) A]
    [(HeterogeneousVector: ts)
     (subtypes* A ts (make-list (length ts) seq-t))]
    [(or (Vector: t) (Set: t)) (subtype* A t seq-t)]
    [(Base: kind _ _ _) #:when (assq kind seq-base-types)
     (subtype* A (cdr (assq kind seq-base-types)) seq-t)]
    [(Value: (? exact-nonnegative-integer? n))
     (define possibilities
       (list
        (list byte? -Byte)
        (list portable-index? -Index)
        (list portable-fixnum? -NonNegFixnum)
        (list values -Nat)))
     (define type
       (for/or ((pred-type (in-list possibilities)))
         (match pred-type
           ((list pred? type)
            (and (pred? n) type)))))
     (subtype* A type seq-t)]
    [(Base: _ _ _ #t)
     (define type
       ;; FIXME: thread the store through here
       (for/or ((num-t (in-list (list -Byte -Index -NonNegFixnum -Nat))))
         (or (and (subtype* A t num-t) num-t))))
     (if type
         (subtype* A type seq-t)
         #f)]
    [_ #f]))


;; event-subtype
;; returns if t is a subtype of (Evt: evt-t)
(define/cond-contract (event-subtype A t evt-t)
  (-> list? Type? Type? (or/c list? #f))
  (match t
    [(Base: kind _ _ _) #:when (memq kind '(Semaphore
                                            Output-Port
                                            Input-Port
                                            TCP-Listener
                                            Thread
                                            Subprocess
                                            Will-Executor))
     (subtype* A t evt-t)]
    ;; FIXME: change Univ to Place-Message-Allowed if/when that type is defined
    [(Base: kind _ _ _) #:when (and (Univ? evt-t)
                                    (memq kind '(Place Base-Place-Channel)))
     A]
    [(Base: 'LogReceiver _ _ _)
     (subtype* A
               (make-HeterogeneousVector
                (list -Symbol -String Univ
                      (Un (-val #f) -Symbol)))
               evt-t)]
    [(CustodianBox: _)
     ;; Note that it's the whole box type that's being
     ;; compared against t* here
     (subtype* A t evt-t)]
    [(or (Channel: t)
         (Async-Channel: t))
     (subtype* A t evt-t)]
    [_ #f]))

