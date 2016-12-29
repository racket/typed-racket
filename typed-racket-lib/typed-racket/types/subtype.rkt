#lang racket/base
(require (except-in "../utils/utils.rkt" infer)
         racket/match racket/function racket/lazy-require
         racket/list racket/set
         (contract-req)
         (rep type-rep prop-rep object-rep
              core-rep type-mask values-rep rep-utils
              free-variance rep-switch)
         (utils tc-utils)
         (types utils resolve match-expanders current-seen
                numeric-tower substitute prefab signatures)
         (for-syntax racket/base syntax/parse racket/sequence)
         (except-in (rename-in "abbrev.rkt"
                               [-> t->]
                               [->* t->*])
                    one-of/c))

(lazy-require
 ("../infer/infer.rkt" (infer))
 ("../typecheck/tc-subst.rkt" (restrict-values)))

(provide NameStruct:)

(provide/cond-contract
 [subtype (-> Type? Type? boolean?)]
 [subresult (-> Result? Result? boolean?)]
 [subval (-> SomeValues? SomeValues? boolean?)]
 [type-equiv? (-> Type? Type? boolean?)]
 [subtypes (-> (listof Type?) (listof Type?) boolean?)]
 [subtypes/varargs (-> (listof Type?) (listof Type?) (or/c Type? #f) boolean?)]
 [unrelated-structs (-> Struct? Struct? boolean?)])


;;************************************************************
;; Public Interface to Subtyping
;;************************************************************

;; is t1 a subtype of t2?
;; type type -> boolean
(define (subtype t1 t2)
  (and (subtype* (seen) t1 t2) #t))


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


;; combine-arrs
;;
;; Checks if this function is defined by an uneccessary case->
;; matching the following pattern:
;; τ0 -> σ ∧ τ1 -> σ ∧ τn -> σ ...
;; and if so, returns the combined function type:
;; (∪ τ0 τ1 ... τn)-> σ
;; amk: would it be better to simplify function types ahead of time
;; for cases like this where there is a preferable normal form?
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
                   _))])))

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

(define/cond-contract (type≡? A t1 t2)
  (-> list? Type? Type? any/c)
  (subtype-seq A
               (subtype* t1 t2)
               (subtype* t2 t1)))

(define union-super-cache (make-weak-hash))
(define union-sub-cache (make-weak-hash))

;; cache-set!
;; caches 'result' as the answer for 't1 <: t2'
(define/cond-contract (cache-set! cache t1 t2 result)
  (-> hash? Type? Type? boolean? void?)
  (hash-set! (hash-ref cache t1 (λ () (make-weak-hash))) t2 (box-immutable result)))

;; cache-ref
;; checks if 't1 <: t2 = b' has already been calculated
;; and if so, returning (box b), otherwise return #f
(define/cond-contract (cache-ref cache t1 t2)
  (-> hash? Type? Type? (or/c #f (box/c boolean?)))
  (cond
    [(hash-ref cache t1 #f)
     => (λ (inner-cache) (hash-ref inner-cache t2 #f))]
    [else #f]))

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
     (match t2
       [(Intersection: t2s)
        (for/fold ([A A])
                  ([t2 (in-list t2s)]
                   #:break (not A))
          (subtype* A t1 t2))]
       [(? resolvable?)
        (let ([A (remember t1 t2 A)])
          (with-updated-seen A
            (let ([t2 (resolve-once t2)])
              ;; check needed for if a name that hasn't been resolved yet
              (and (Type? t2) (subtype* A t1 t2)))))]
       [_
        ;; then we try a switch on t1
        (subtype-switch
         t1 t2 A
         ;; if we're still not certain after the switch,
         ;; check the cases that need to come at the end
         (λ (A t1 t2)
           (match* (t1 t2)
             [(t1 (Union/set: base2 ts2 elems2))
              (cond
                [(set-member? elems2 t1) A]
                [(cache-ref union-super-cache t2 t1)
                 => (λ (b) (and (unbox b) A))]
                [else
                 (define result
                   (or (subtype* A t1 base2)
                       (for/or ([elem (in-list ts2)])
                         (subtype* A t1 elem))))
                 (when (null? A)
                   (cache-set! union-super-cache t2 t1 (and result #t)))
                 result])]
             [((Intersection: t1s) _)
              (for/or ([t1 (in-list t1s)])
                (subtype* A t1 t2))]
             [(_ (Instance: (? resolvable? t2*)))
              (let ([A (remember t1 t2 A)])
                (with-updated-seen A
                  (let ([t2* (resolve-once t2*)])
                    (and (Type? t2*)
                         (subtype* A t1 (make-Instance t2*))))))]
             [(_ (Poly: vs2 b2))
              #:when (null? (fv b2))
              (subtype* A t1 b2)]
             [(_ (PolyDots: vs2 b2))
              #:when (and (null? (fv b2))
                          (null? (fi b2)))
              (subtype* A t1 b2)]
             [(_ _) #f])))])]))


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

(define-switch (subtype-switch t1 t2 A continue)
  ;; NOTE: keep these in alphabetical order
  ;; or ease of finding cases
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
     [_ (continue A t1 t2)])]
  [(case: Base (Base-bits: num? bits))
   (match t2
     [(BaseUnion: bbits nbits)
      (and (if num?
               (nbits-overlap? nbits bits)
               (bbits-overlap? bbits bits))
           A)]
     [(Sequence: (list seq-t))
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
     [_ (continue A t1 t2)])]
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
     [_
      (cond
        [(cache-ref union-sub-cache t1 t2)
         => (λ (b) (and (unbox b) A))]
        [else
         (define result
           (for/fold ([A A])
                     ([b (in-list (BaseUnion-bases t1))]
                      #:break (not A))
             (subtype* A b t2)))
         (when (null? A)
           (cache-set! union-sub-cache t1 t2 (and result #t)))
         result])])]
  [(case: Box (Box: elem1))
   (match t2
     [(? BoxTop?) A]
     [(Box: elem2) (type≡? A elem1 elem2)]
     [_ (continue A t1 t2)])]
  [(case: Channel (Channel: elem1))
   (match t2
     [(? ChannelTop?) A]
     [(Channel: elem2) (type≡? A elem1 elem2)]
     [(Evt: evt-t) (subtype* A elem1 evt-t)]
     [_ (continue A t1 t2)])]
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
     [_ (continue A t1 t2)])]
  [(case: Continuation-Mark-Keyof (Continuation-Mark-Keyof: val1))
   (match t2
     [(? Continuation-Mark-KeyTop?) A]
     [(Continuation-Mark-Keyof: val2)
      (type≡? A val1 val2)]
     [_ (continue A t1 t2)])]
  [(case: CustodianBox (CustodianBox: elem1))
   (match t2
     [(CustodianBox: elem2) (subtype* A elem1 elem2)]
     [(Evt: evt-t)
      ;; Note that it's the whole box type that's being
      ;; compared against evt-t here
      (subtype* A t1 evt-t)]
     [_ (continue A t1 t2)])]
  [(case: Distinction (Distinction: nm1 id1 t1*))
   (match t2
     [(app resolve (Distinction: nm2 id2 t2*))
      #:when (and (equal? nm1 nm2) (equal? id1 id2))
      (subtype* A t1* t2*)]
     [_ (cond
          [(subtype* A t1* t2)]
          [else (continue A t1 t2)])])]
  [(case: Ephemeron (Ephemeron: elem1))
   (match t2
     [(Ephemeron: elem2) (subtype* A elem1 elem2)]
     [_ (continue A t1 t2)])]
  [(case: Evt (Evt: result1))
   (match t2
     [(Evt: result2) (subtype* A result1 result2)]
     [_ (continue A t1 t2)])]
  [(case: F (F: var1))
   (match t2
     ;; tvars are equal if they are the same variable
     [(F: var2) (eq? var1 var2)]
     [_ (continue A t1 t2)])]
  [(case: Function (Function: arrs1))
   (match t2
     ;; special-case for case-lambda/union with only one argument              
     [(Function: (list arr2))
      (cond [(null? arrs1) #f]
            [else
             (define comb (combine-arrs arrs1))
             (or (and comb (arr-subtype*/no-fail A comb arr2))
                 (supertype-of-one/arr A arr2 arrs1))])]
     ;; case-lambda
     [(Function: arrs2)
      (if (null? arrs1) #f
          (let loop-arities ([A A]
                             [arrs2 arrs2])
            (cond
              [(null? arrs2) A]
              [(supertype-of-one/arr A (car arrs2) arrs1)
               => (λ (A) (loop-arities A (cdr arrs2)))]
              [else #f])))]
     [_ (continue A t1 t2)])]
  [(case: Future (Future: elem1))
   (match t2
     [(Future: elem2) (subtype* A elem1 elem2)]
     [_ (continue A t1 t2)])]
  [(case: Hashtable (Hashtable: key1 val1))
   (match t2
     [(? HashtableTop?) A]
     [(Hashtable: key2 val2) (subtype-seq A
                                          (type≡? key1 key2)
                                          (type≡? val1 val2))]
     [(Sequence: (list key2 val2))
      (subtype-seq A
                   (subtype* key1 key2)
                   (subtype* val1 val2))]
     [_ (continue A t1 t2)])]
  [(case: HeterogeneousVector (HeterogeneousVector: elems1))
   (match t2
     [(VectorTop:) A]
     [(HeterogeneousVector: elems2)
      (cond [(= (length elems1)
                (length elems2))
             (for/fold ([A A])
                       ([elem1 (in-list elems1)]
                        [elem2 (in-list elems2)]
                        #:break (not A))
               (type≡? A elem1 elem2))]
            [else #f])]
     [(Vector: elem2)
      (for/fold ([A A])
                ([elem1 (in-list elems1)] #:break (not A))
        (type≡? A elem1 elem2))]
     [(Sequence: (list seq-t))
      (for/fold ([A A])
                ([elem1 (in-list elems1)]
                 #:break (not A))
        (subtype* A elem1 seq-t))]
     [_ (continue A t1 t2)])]
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
        [(_ _) (continue A t1 t2)])])]
  [(case: Intersection (Intersection: t1s))
   (cond
     [(for/or ([t1 (in-list t1s)])
        (subtype* A t1 t2))]
     [else (continue A t1 t2)])]
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
     [_ (continue A t1 t2)])]
  [(case: MPair (MPair: t11 t12))
   (match t2
     [(? MPairTop?) A]
     [(MPair: t21 t22)
      (subtype-seq A
                   (type≡? t11 t21)
                   (type≡? t12 t22))]
     ;; To check that mutable pair is a sequence we check that the cdr
     ;; is both an mutable list and a sequence 
     [(Sequence: (list seq-t))
      (subtype-seq A
                   (subtype* t11 seq-t)
                   (subtype* t12 null-or-mpair-top)
                   (subtype* t12 (make-Sequence (list seq-t))))]
     [_ (continue A t1 t2)])]
  [(case: Mu _)
   (let ([A (remember t1 t2 A)])
     (with-updated-seen A
       (let ([t1 (unfold t1)])
         ;; check needed for if a name that hasn't been resolved yet
         (and (Type? t1) (subtype* A t1 t2)))))]
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
                   (subtype* t11 t21)
                   (subtype* t12 t22))]
     [(Sequence: (list seq-t))
      (subtype-seq A
                   (subtype* t11 seq-t)
                   (subtype* t12 (-lst seq-t)))]
     [_ (continue A t1 t2)])]
  [(case: Param (Param: in1 out1))
   (match t2
     [(Param: in2 out2) (subtype-seq A
                                     (subtype* in2 in1)
                                     (subtype* out1 out2))]
     [_ (subtype* A (cl->* (t-> out1) (t-> in1 -Void)) t2)]
     [_ (continue A t1 t2)])]
  [(case: Poly (Poly: names b1))
   (match t2
     [(? Poly?) #:when (= (length names) (Poly-n t2))
                (subtype* A b1 (Poly-body names t2))]
     ;; use local inference to see if we can use the polytype here
     [_ #:when (infer names null (list b1) (list t2) Univ) A]
     [_ (continue A t1 t2)])]
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
     [_ (continue A t1 t2)])]
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
     [_ (continue A t1 t2)])]
  [(case: Promise (Promise: elem1))
   (match t2
     [(Promise: elem2) (subtype* A elem1 elem2)]
     [_ (continue A t1 t2)])]
  [(case: Prompt-Tagof (Prompt-Tagof: body1 handler1))
   (match t2
     [(? Prompt-TagTop?) A]
     [(Prompt-Tagof: body2 handler2)
      (subtype-seq A
                   (type≡? body1 body2)
                   (type≡? handler1 handler2))]
     [_ (continue A t1 t2)])]
  [(case: Refinement (Refinement: t1-parent id1))
   (match t2
     [(Refinement: t2-parent id2)
      #:when (free-identifier=? id1 id2)
      (subtype* A t1-parent t2-parent)]
     [_ (cond
          [(subtype* A t1-parent t2)]
          [else (continue A t1 t2)])])]
  ;; sequences are covariant
  [(case: Sequence (Sequence: ts1))
   (match t2
     [(Sequence: ts2) (subtypes* A ts1 ts2)]
     [_ (continue A t1 t2)])]
  [(case: Set (Set: elem1))
   (match t2
     [(Set: elem2) (subtype* A elem1 elem2)]
     [(Sequence: (list seq-t)) (subtype* A elem1 seq-t)]
     [_ (continue A t1 t2)])]
  [(case: Struct (Struct: nm1 parent1 flds1 proc1 _ _))
   (match t2
     ;; Avoid resolving things that refer to different structs.
     ;; Saves us from non-termination
     [(or (? Struct? t2) (NameStruct: t2))
      #:when (unrelated-structs t1 t2)
      #f]
     ;; subtyping on immutable structs is covariant
     [(Struct: nm2 _ flds2 proc2 _ _)
      #:when (free-identifier=? nm1 nm2)
      (let ([A (remember t1 t2 A)])
        (with-updated-seen A
          (let ([A (cond [(and proc1 proc2) (subtype* A proc1 proc2)]
                         [proc2 #f]
                         [else A])])
            (and A (subtype/flds* A flds1 flds2)))))]
     [(StructTop: (Struct: nm2 _ _ _ _ _))
      #:when (free-identifier=? nm1 nm2)
      A]
     [(Val-able: (? (negate struct?) _)) #f]
     ;; subtyping on structs follows the declared hierarchy
     [_ (cond
          [(and (Type? parent1)
                (let ([A (remember t1 t2 A)])
                  (with-updated-seen A
                    (subtype* A parent1 t2))))]
          [else (continue A t1 t2)])]
     [_ (continue A t1 t2)])]
  [(case: StructType (StructType: t1*))
   (match t2
     [(StructTypeTop:) A]
     [_ (continue A t1 t2)])]
  [(case: Syntax (Syntax: elem1))
   (match t2
     [(Syntax: elem2) (subtype* A elem1 elem2)]
     [_ (continue A t1 t2)])]
  [(case: ThreadCell (ThreadCell: elem1))
   (match t2
     [(? ThreadCellTop?) A]
     [(ThreadCell: elem2) (type≡? A elem1 elem2)]
     [_ (continue A t1 t2)])]
  [(case: Union (Union/set: base1 ts1 elems1))
   (cond
     [(cache-ref union-sub-cache t1 t2)
      => (λ (b) (and (unbox b) A))]
     [else
      (define result
        (let ([A (subtype* A base1 t2)])
          (and A
               (match t2
                 [(Union/set: base2 ts2 elems2)
                  (for/fold ([A A])
                            ([elem1 (in-list ts1)]
                             #:break (not A))
                    (cond
                      [(set-member? elems2 elem1) A]
                      [(subtype* A elem1 base2)]
                      [else (subtype* A elem1 t2)]))]
                 [_ (for/fold ([A A])
                              ([elem1 (in-list ts1)]
                               #:break (not A))
                      (subtype* A elem1 t2))]))))
      (when (null? A)
        (cache-set! union-sub-cache t1 t2 (and result #t)))
      result])]
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
     [_ (continue A t1 t2)])]
  [(case: Value (Value: val1))
   (match t2
     [(Base-predicate: pred) (and (pred val1) A)]
     [(BaseUnion-bases: bs)
      (for*/or ([b (in-list bs)]
                [pred (in-value (Base-predicate b))])
        (and (pred val1) A))]
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
     [_ (continue A t1 t2)])]
  [(case: Vector (Vector: elem1))
   (match t2
     [(? VectorTop?) A]
     [(Vector: elem2) (type≡? A elem1 elem2)]
     [(Sequence: (list seq-t)) (subtype* A elem1 seq-t)]
     [_ (continue A t1 t2)])]
  [(case: Weak-Box (Weak-Box: elem1))
   (match t2
     [(? Weak-BoxTop?) A]
     [(Weak-Box: elem2) (type≡? A elem1 elem2)]
     [_ (continue A t1 t2)])]
  [else: (continue A t1 t2)])
