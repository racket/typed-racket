#lang racket/base

(require "../utils/utils.rkt"
         racket/list racket/match
         (prefix-in c: (contract-req))
         (rep type-rep prop-rep object-rep values-rep rep-utils)
         (logic ineq)
         (only-in (infer infer) intersect)
         (types subtype overlap subtract abbrev tc-result union path-type update))

(provide/cond-contract
  [-and (c:->* () #:rest (c:listof Prop?) Prop?)]
  [-or (c:->* () #:rest (c:listof Prop?) Prop?)]
  [atomic-implies? (c:-> Prop? Prop? boolean?)]
  [implies? (c:-> Prop? Prop? boolean?)]
  [prop-equiv? (c:-> Prop? Prop? boolean?)]
  [negate-prop (c:-> Prop? Prop?)]
  [atomic-complement? (c:-> Prop? Prop? boolean?)]
  [atomic-contradiction? (c:-> Prop? Prop? boolean?)]
  [contradiction? (c:-> Prop? Prop? boolean?)]
  [add-unconditional-prop-all-args (c:-> Fun? Type? Fun?)]
  [add-unconditional-prop (c:-> tc-results/c Prop? tc-results/c)]
  [erase-props (c:-> tc-results/c tc-results/c)]
  [reduce-propset/type (c:-> PropSet? Type? PropSet?)]
  [reduce-tc-results/subsumption (c:-> tc-results/c tc-results/c)])

;; reduces a PropSet 'ps' with info from the type 't'
;; so the two are consistent (e.g. if the type is False,
;; its true proposition is -ff, etc)
(define (reduce-propset/type ps t)
  (cond
    [(Bottom? t) -ff-propset]
    [(equal? -False t) (-PS -ff (PropSet-els ps))]
    [(not (overlap? t -False)) (-PS (PropSet-thn ps) -ff)]
    [else ps]))

;; reduce-tc-result/subsumption
;;
;; tc-result -> tc-result
;;
;; Update the tc-result to incorporate the
;; return type in the proposition (i.e. if it
;; can't be False, then the else prop should be -ff)
(define (reduce-tc-results/subsumption res)
  (define (update-ps tcr)
    (match tcr
      [(tc-result: t ps obj)
       (cond
         [(Bottom? t) (-tc-result t -ff-propset -empty-obj)]
         [else
          (define p+ (if ps (PropSet-thn ps) -tt))
          (define p- (if ps (PropSet-els ps) -tt))
          (define o (if obj obj -empty-obj))
          (cond
            [(or (equal? -False t)
                 (FalseProp? p+))
             (-tc-result (intersect t -False) (-PS -ff p-) o)]
            [(not (overlap? t -False))
             (-tc-result t (-PS p+ -ff) o)]
            [(FalseProp? p-) (-tc-result (subtract t -False) (-PS p+ -ff) o)]
            [else (-tc-result t (-PS p+ p-) o)])])]))
  (match res
    [(tc-any-results: _) res]
    [(tc-results: tcrs db)
     (-tc-results (map update-ps tcrs) db)]))


;; atomic-contradiction?: Prop? Prop? -> boolean?
;; Returns true if the AND of the two props is equivalent to FalseProp
(define/match (atomic-contradiction? p1 p2)
  [((TypeProp: o1 t1) (NotTypeProp: o2 t2))
   (and (eq? o1 o2) (subtype t1 t2 o1))]
  [((NotTypeProp: o2 t2) (TypeProp: o1 t1))
   (and (eq? o1 o2) (subtype t1 t2 o1))]
  [((? LeqProp?) (? LeqProp?)) (contradictory-Leqs? p1 p2)]
  [((FalseProp:) _) #t]
  [(_ (FalseProp:)) #t]
  [(_ _) #f])


;; do pes1 and pes2 share a common suffix and,
;; if so, does updating the shorter w/ the longer
;; result in bottom?
;; e.g. For the following args:
;;      pes1 = (car (cdr x))
;;      t1   = String
;;      pes2 = (cdr x)
;;      t2   = (Pairof Symbol Symbol)
;; it would see that both sets are talking
;; about (cdr x), and the first path actually
;; talks about a structural subcomponent of that
;; so it would update the 'car' of (Pairof Symbol Symbol)
;; with String and see that that is Bottom,
;; so it would return #t
(define (updates/pos-to-bot? pes1 t1 pes2 t2)
  (define (check pes1 t1 pes2 t2 prefix-len)
    (and (equal? pes1 (drop pes2 prefix-len))
         (let ([prefix (take pes2 prefix-len)])
           (Bottom? (update t1 t2 #t prefix)))))
  (let ([len1 (length pes1)]
        [len2 (length pes2)])
    (cond
      [(<= len1 len2)
       (check pes1 t1 pes2 t2 (- len2 len1))]
      [else
       (check pes2 t2 pes1 t1 (- len1 len2))])))

;; does pes2 refer to the same or a subcomponent of
;; pes1 and if so, does updating the t1+ w/ the t2-
;; alond the difference result in bottom?
;; e.g. For the following args:
;;      pes1 = (cdr x)
;;      t1+  = (Pairof Symbol Symbol)
;;      pes2 = (car (cdr x))
;;      t2-  = Symbol
;; would basically see that the path elements
;; are compatible and would update the car
;; field of (Pairof Symbol Symbol) to be _not_
;; Symbol, and so it would return #t
(define (updates/neg-to-bot? pes1 t1+ pes2 t2-)
  (define len1 (length pes1))
  (define len2 (length pes2))
  (and (<= len1 len2)
       (let ([prefix-len (- len2 len1)])
         (and (equal? pes1 (drop pes2 prefix-len))
              (let ([prefix (take pes2 prefix-len)])
                (Bottom? (update t1+ t2- #f prefix)))))))

;; like atomic-contradiction? but it tries a little
;; harder, reasoning about how paths overlap
(define/match (contradiction? p1 p2)
  [((FalseProp:) _) #t]
  [(_ (FalseProp:)) #t]
  [((TypeProp: o t1) (NotTypeProp: o t2)) (subtype t1 t2 o)]
  [((NotTypeProp: o t2) (TypeProp: o t1)) (subtype t1 t2 o)]
  [((? LeqProp?) (? LeqProp?)) (contradictory-Leqs? p1 p2)]
  [((TypeProp: (Path: pes1 x1) t1)
    (TypeProp: (Path: pes2 x2) t2))
   #:when (name-ref=? x1 x2)
   (updates/pos-to-bot? pes1 t1 pes2 t2)]
  [((TypeProp:    (Path: pes1 x1) t1)
    (NotTypeProp: (Path: pes2 x2) t2))
   #:when (name-ref=? x1 x2)
   (updates/neg-to-bot? pes1 t1 pes2 t2)]
  [((NotTypeProp: (Path: pes2 x2) t2)
    (TypeProp:    (Path: pes1 x1) t1))
   #:when (name-ref=? x1 x2)
   (updates/neg-to-bot? pes1 t1 pes2 t2)]
  [(_ _) #f])


;; atomic-complement?: Prop? Prop? -> boolean?
;; Returns true if the OR of the two props is equivalent to Top
(define/match (atomic-complement? p1 p2)
  [((TypeProp: o1 t1) (NotTypeProp: o2 t2))
   (and (eq? o1 o2) (subtype t2 t1 o1))]
  [((NotTypeProp: o2 t2) (TypeProp: o1 t1))
   (and (eq? o1 o2) (subtype t2 t1 o1))]
  [((? LeqProp?) (? LeqProp?)) (complementary-Leqs? p1 p2)]
  [((TrueProp:) _) #t]
  [(_ (TrueProp:)) #t]
  [(_ _) #f])

;; does p imply q? (but only directly/simply)
;; NOTE: because Ors and Atomic props are
;; interned, we use eq? and memq
(define/match (atomic-implies? p q)
  ;; reflexivity
  [(_ _) #:when (or (eq? p q)
                    (TrueProp? q)
                    (FalseProp? p)) #t]
  ;; ps ⊆ qs ?
  [((OrProp: ps) (OrProp: qs))
   (and (for/and ([p (in-list ps)])
          (memq p qs))
        #t)]
  ;; p ∈ qs ?
  [(p (OrProp: qs)) (and (memq p qs) #t)]
  ;; q ∈ ps ?
  [((AndProp: ps) q) (or (equal? p q) (and (memq q ps) #t))]
  ;; t1 <: t2 ?
  [((TypeProp: o1 t1)
    (TypeProp: o2 t2))
   (and (eq? o1 o2) (subtype t1 t2 o1))]
  ;; t2 <: t1 ?
  [((NotTypeProp: o1 t1)
    (NotTypeProp: o2 t2))
   (and (eq? o1 o2) (subtype t2 t1 o1))]
  ;; t1 ∩ t2 = ∅ ?
  [((TypeProp:    o1 t1)
    (NotTypeProp: o2 t2))
   (and (eq? o1 o2) (not (overlap? t1 t2)))]
  [((? LeqProp? p) (? LeqProp? q))
   (Leq-implies-Leq? p q)]
  ;; otherwise we give up
  [(_ _) #f])

(define (implies? p q)
  (FalseProp? (-and p (negate-prop q))))

(define (prop-equiv? p q)
  (and (implies? p q)
       (implies? q p)))

;; helpers for compact 
(define (intersect-update! dict t1 p)
  (hash-update! dict p (λ (t2) (intersect t1 t2)) Univ))
(define (union-update! dict t1 p)
  (hash-update! dict p (λ (t2) (Un t1 t2)) -Bottom))

;; compact : (listof prop) bool -> (listof prop)
;; props : propositions to compress
;; or? : is this an Or (alternative is And)
;;
;; This combines all the TypeProps at the same path into one TypeProp. If it is an Or the
;; combination is done using Un, otherwise, intersect. The reverse is done for NotTypeProps.
;; If it is an Or this simplifies to -tt if any of the atomic props simplified to -tt, and
;; removes any -ff values. The reverse is done if this is an And.
;;
;; NOTE: this is significantly faster as a macro than a function (even
;; with define-inline)
(define-syntax-rule (compact props or?)
  (match props
    [(or (list) (list _)) props]
    [_
     (define tf-map (make-hasheq))
     (define ntf-map (make-hasheq))

     ;; consolidate type info and separate out other props
     (define-values (leqs others)
       (for/fold ([leqs '()]
                  [others '()])
                 ([prop (in-list props)])
         (match prop
           [(TypeProp: o t1)
            ((if or? union-update! intersect-update!) tf-map t1 o)
            (values leqs others)]
           [(NotTypeProp: o t1)
            ((if or? intersect-update! union-update!) ntf-map t1 o)
            (values leqs others)]
           [(? LeqProp? p) (values (cons p leqs) others)]
           [_ (values leqs (cons prop others))])))
     ;; convert consolidated types into props and gather everything
     (define raw-results
       (append (for/list ([(k v) (in-mutable-hash tf-map)])
                 (-is-type k v))
               (for/list([(k v) (in-mutable-hash ntf-map)])
                 (-not-type k v))
               leqs
               others))
     ;; check for abort condition and remove trivial props
     (cond
       [or?
        (if (member -tt raw-results)
            (list -tt)
            (filter-not FalseProp? raw-results))]
       [else
        (cond
          [(or (member -ff raw-results)
               (not (satisfiable-Leqs? leqs)))
           (list -ff)]
          [else (filter-not TrueProp? raw-results)])])]))



;; negate-prop: Prop? -> Prop?
;; Logically inverts a prop.
(define/match (negate-prop p)
  [((? FalseProp?)) -tt]
  [((? TrueProp?)) -ff]
  [((TypeProp: o t)) (-not-type o t)]
  [((NotTypeProp: o t)) (-is-type o t)]
  [((AndProp: ps)) (apply -or (map negate-prop ps))]
  [((OrProp: ps)) (apply -and (map negate-prop ps))]
  [((LeqProp: lhs rhs))
   (-leq (-lexp-add1 rhs) lhs)])

;; -or
;; (listof Prop?) -> Prop?
;;
;; Smart 'normalizing' constructor for disjunctions. The result
;; will be a disjunction of only atomic propositions (i.e. a clause
;; in a CNF formula)
(define/match (-or . args)
  ;; specialize for trivial <=2 arg cases
  [((list)) -ff]
  [((list p)) p]
  [((list p p)) p]
  [((list (TrueProp:) p)) -tt]
  [((list p (TrueProp:))) -tt]
  [((list (FalseProp:) p)) p]
  [((list p (FalseProp:))) p]
  [(args)
   (define mk
     (match-lambda [(list) -ff]
                   [(list p) p]
                   [ps (make-OrProp ps)]))
   (define (distribute args)
     (define-values (ands others) (partition AndProp? args))
     (if (null? ands)
         (mk others)
         (match-let ([(AndProp: elems) (car ands)])
           (apply -and (for/list ([a (in-list elems)])
                         (apply -or a (append (cdr ands) others)))))))
   (define (flatten-ors/remove-duplicates ps)
     (let loop ([ps ps]
                [result '()])
       (match ps
         [(cons p rst)
          (match p
            [(OrProp: ps*) (loop rst (append ps* result))]
            [_ (loop rst (cons p result))])]
         [_ (remove-duplicates result)])))
   (let loop ([ps (flatten-ors/remove-duplicates args)]
              [result null])
     (match ps
       [(cons cur rst)
        (cond
          ;; trivial cases
          [(TrueProp? cur) -tt]
          [(FalseProp? cur) (loop rst result)]
          ;; is there a complementary case e.g. (ϕ ∨ ¬ϕ)? if so abort
          [(for/or ([p (in-list rst)])    (atomic-complement? p cur)) -tt]
          [(for/or ([p (in-list result)]) (atomic-complement? p cur)) -tt]
          ;; don't include 'cur' if its covered by another prop
          [(for/or ([p (in-list rst)]) (atomic-implies? cur p))
           (loop rst result)]
          [(for/or ([p (in-list result)]) (atomic-implies? cur p))
           (loop rst result)]
          ;; otherwise keep 'cur' in this disjunction
          [else (loop rst (cons cur result))])]
       [_ (distribute (compact result #t))]))])

;; -and
;; (listof Prop?) -> Prop?
;;
;; Smart 'normalizing' constructor for conjunctions. The result
;; will be a conjunction of only atomic propositions and disjunctions
;; (i.e. a CNF proposition)
(define/match (-and . args)
  ;; specialize for trivial <=2 arg cases
  [((list)) -tt]
  [((list p)) p]
  [((list p p)) p]
  [((list (TrueProp:) p)) p]
  [((list p (TrueProp:))) p]
  [((list (FalseProp:) p)) -ff]
  [((list p (FalseProp:))) -ff]
  [(args)
   (define mk
     (match-lambda [(list) -tt]
                   [(list p) p]
                   [ps (make-AndProp ps)]))
   ;; we remove duplicates and organize the props so that the
   ;; strongest ones come first (note: this includes considering
   ;; smaller ors before larger ors)
   (define (flatten-ands/remove-duplicates/order ps)
     (define ts '())
     (define nts '())
     (define ors (make-hash))
     (define others '())
     (let partition! ([ps ps])
       (for ([p (in-list ps)])
         (match p
           [(? TypeProp?) (set! ts (cons p ts))]
           [(? NotTypeProp?) (set! nts (cons p nts))]
           [(OrProp: ps*) (hash-update! ors (length ps*) (λ (l) (cons p l)) '())]
           [(AndProp: ps*) (partition! ps*)]
           [_ (set! others (cons p others))])))
     (define ors-smallest-to-largest
       (append-map cdr (sort (hash->list ors)
                             (λ (len/ors1 len/ors2)
                               (< (car len/ors1) (car len/ors2))))))
     (remove-duplicates (append ts nts others ors-smallest-to-largest) eq?))
   (let loop ([ps (flatten-ands/remove-duplicates/order args)]
              [result null])
     (match ps
       [(cons cur rst)
        (cond
          ;; trivial cases
          [(FalseProp? cur) -ff]
          [(TrueProp? cur) (loop rst result)]
          ;; is there a contradition e.g. (ϕ ∧ ¬ϕ), if so abort
          [(for/or ([p (in-list rst)])    (atomic-contradiction? p cur)) -ff]
          [(for/or ([p (in-list result)]) (atomic-contradiction? p cur)) -ff]
          ;; don't include 'cur' if its implied by another prop
          ;; already in our result (this is why we order the props!)
          [(for/or ([p (in-list result)]) (atomic-implies? p cur))
           (loop rst result)]
          ;; otherwise keep 'cur' in this conjunction
          [else (loop rst (cons cur result))])]
       [_ (mk (compact result #f))]))])

;; add-unconditional-prop: tc-results? Prop? -> tc-results?
;; Ands the given proposition to the props in the tc-results.
;; Useful to express properties of the form: if this expressions returns at all, we learn this
(define/match (add-unconditional-prop results prop)
  [((tc-any-results: p) prop) (-tc-any-results (-and prop p))]
  [((tc-results: tcrs db) prop)
   (-tc-results
    (map (match-lambda
           [(tc-result: t (PropSet: p+ p-) o)
            (-tc-result t (-PS (-and prop p+) (-and prop p-)) o)])
         tcrs)
    db)])


;; ands the given type prop to both sides of the given arr for each argument
;; useful to express properties of the form: if this function returns at all,
;; we learn this about its arguments (like fx primitives, or car/cdr, etc.)
(define/match (add-unconditional-prop-all-args arr type)
  [((Fun: (list (Arrow: dom rst kws rng))) type)
   (match rng
     [(Values: (list (Result: tp (PropSet: p+ p-) op)))
      (let ([new-props (apply -and (build-list (length dom)
                                               (lambda (i)
                                                 (-is-type i type))))])
        (make-Fun
         (list (make-Arrow dom rst kws
                           (make-Values
                            (list (-result tp
                                           (-PS (-and p+ new-props)
                                                (-and p- new-props))
                                           op)))))))])])

;; tc-results/c -> tc-results/c
(define/match (erase-props tc)
  [((tc-any-results: _)) (-tc-any-results #f)]
  [((tc-results: tcrs dbound))
   (-tc-results
    (map (match-lambda
           [(tc-result: t) (-tc-result t #f #f)])
         tcrs)
    dbound)])
