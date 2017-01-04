#lang racket/base

(require "../utils/utils.rkt"
         racket/list racket/match
         (prefix-in c: (contract-req))
         (rep type-rep prop-rep object-rep values-rep rep-utils)
         (only-in (infer infer) intersect)
         (types subtype overlap subtract abbrev tc-result union))

(provide/cond-contract
  [-and (c:->* () #:rest (c:listof Prop?) Prop?)]
  [-or (c:->* () #:rest (c:listof Prop?) Prop?)]
  [implies-atomic? (c:-> Prop? Prop? boolean?)]
  [implies? (c:-> Prop? Prop? boolean?)]
  [prop-equiv? (c:-> Prop? Prop? boolean?)]
  [negate-prop (c:-> Prop? Prop?)]
  [complementary? (c:-> Prop? Prop? boolean?)]
  [contradictory? (c:-> Prop? Prop? boolean?)]
  [add-unconditional-prop-all-args (c:-> Function? Type? Function?)]
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
  (define (update-ps t ps obj)
    (cond
      [(Bottom? t) (tc-result t -ff-propset -empty-obj)]
      [else
       (define p+ (if ps (PropSet-thn ps) -tt))
       (define p- (if ps (PropSet-els ps) -tt))
       (define o (if obj obj -empty-obj))
       (cond
         [(or (equal? -False t)
              (FalseProp? p+))
          (tc-result (intersect t -False) (-PS -ff p-) o)]
         [(not (overlap? t -False))
          (tc-result t (-PS p+ -ff) o)]
         [(FalseProp? p-) (tc-result (subtract t -False) (-PS p+ -ff) o)]
         [else (tc-result t (-PS p+ p-) o)])]))
  (match res
    [(tc-any-results: _) res]
    [(tc-results: ts pss os)
     (tc-results (map update-ps ts pss os) #f)]
    [(tc-results: ts pss os dt db)
     (tc-results (map update-ps ts pss os) (cons dt db))]
    [_ (error 'reduce-tc-results/subsumption
              "invalid res in subst-tc-results: ~a"
              res)]))


;; contradictory: Prop? Prop? -> boolean?
;; Returns true if the AND of the two props is equivalent to FalseProp
(define (contradictory? p1 p2)
  (match* (p1 p2)
    [((TypeProp: o1 t1) (NotTypeProp: o2 t2))
     (and (eq? o1 o2) (subtype t1 t2))]
    [((NotTypeProp: o2 t2) (TypeProp: o1 t1))
     (and (eq? o1 o2) (subtype t1 t2))]
    [((FalseProp:) _) #t]
    [(_ (FalseProp:)) #t]
    [(_ _) #f]))

;; complementary: Prop? Prop? -> boolean?
;; Returns true if the OR of the two props is equivalent to Top
(define (complementary? p1 p2)
  (match* (p1 p2)
    [((TypeProp: o1 t1) (NotTypeProp: o2 t2))
     (and (eq? o1 o2) (subtype t2 t1))]
    [((NotTypeProp: o2 t2) (TypeProp: o1 t1))
     (and (eq? o1 o2) (subtype t2 t1))]
    [((TrueProp:) _) #t]
    [(_ (TrueProp:)) #t]
    [(_ _) #f]))

;; does p imply q? (but only directly/simply)
;; NOTE: because Ors and Atomic props are
;; interned, we use eq? and memq
(define (implies-atomic? p q)
  (match* (p q)
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
     (and (eq? o1 o2) (subtype t1 t2))]
    ;; t2 <: t1 ?
    [((NotTypeProp: o1 t1)
      (NotTypeProp: o2 t2))
     (and (eq? o1 o2) (subtype t2 t1))]
    ;; t1 ∩ t2 = ∅ ?
    [((TypeProp:    o1 t1)
      (NotTypeProp: o2 t2))
     (and (eq? o1 o2) (not (overlap? t1 t2)))]
    ;; otherwise we give up
    [(_ _) #f]))

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
     (define tf-map (make-hash))
     (define ntf-map (make-hash))

     ;; consolidate type info and separate out other props
     (define others
       (for/fold ([others '()])
                 ([prop (in-list props)])
         (match prop
           [(TypeProp: o t1)
            ((if or? union-update! intersect-update!) tf-map t1 o)
            others]
           [(NotTypeProp: o t1)
            ((if or? intersect-update! union-update!) ntf-map t1 o)
            others]
           [_ (cons prop others)])))
     ;; convert consolidated types into props and gather everything
     (define raw-results
       (append (for/list ([(k v) (in-hash tf-map)])
                 (-is-type k v))
               (for/list([(k v) (in-hash ntf-map)])
                 (-not-type k v))
               others))
     ;; check for abort condition and remove trivial props
     (if or?
         (if (member -tt raw-results)
             (list -tt)
             (filter-not FalseProp? raw-results))
         (if (member -ff raw-results)
             (list -ff)
             (filter-not TrueProp? raw-results)))]))



;; negate-prop: Prop? -> Prop?
;; Logically inverts a prop.
(define (negate-prop p)
  (match p
    [(? FalseProp?) -tt]
    [(? TrueProp?) -ff]
    [(TypeProp: o t) (-not-type o t)]
    [(NotTypeProp: o t) (-is-type o t)]
    [(AndProp: ps) (apply -or (map negate-prop ps))]
    [(OrProp: ps) (apply -and (map negate-prop ps))]))

;; -or
;; (listof Prop?) -> Prop?
;;
;; Smart 'normalizing' constructor for disjunctions. The result
;; will be a disjunction of only atomic propositions (i.e. a clause
;; in a CNF formula)
(define (-or . args)
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
         [(for/or ([p (in-list rst)])    (complementary? p cur)) -tt]
         [(for/or ([p (in-list result)]) (complementary? p cur)) -tt]
         ;; don't include 'cur' if its covered by another prop
         [(for/or ([p (in-list rst)]) (implies-atomic? cur p))
          (loop rst result)]
         [(for/or ([p (in-list result)]) (implies-atomic? cur p))
          (loop rst result)]
         ;; otherwise keep 'cur' in this disjunction
         [else (loop rst (cons cur result))])]
      [_ (distribute (compact result #t))])))

;; -and
;; (listof Prop?) -> Prop?
;;
;; Smart 'normalizing' constructor for conjunctions. The result
;; will be a conjunction of only atomic propositions and disjunctions
;; (i.e. a CNF proposition)
(define (-and . args)
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
         [(for/or ([p (in-list rst)])    (contradictory? p cur)) -ff]
         [(for/or ([p (in-list result)]) (contradictory? p cur)) -ff]
         ;; don't include 'cur' if its implied by another prop
         ;; already in our result (this is why we order the props!)
         [(for/or ([p (in-list result)]) (implies-atomic? p cur))
          (loop rst result)]
         ;; otherwise keep 'cur' in this conjunction
         [else (loop rst (cons cur result))])]
      [_ (mk (compact result #f))])))

;; add-unconditional-prop: tc-results? Prop? -> tc-results?
;; Ands the given proposition to the props in the tc-results.
;; Useful to express properties of the form: if this expressions returns at all, we learn this
(define (add-unconditional-prop results prop)
  (match results
    [(tc-any-results: p) (tc-any-results (-and prop p))]
    [(tc-results: ts (list (PropSet: ps+ ps-) ...) os)
     (ret ts
          (for/list ([p+ (in-list ps+)]
                     [p- (in-list ps-)])
            (-PS (-and prop p+) (-and prop p-)))
          os)]
    [(tc-results: ts (list (PropSet: ps+ ps-) ...) os dty dbound)
     (ret ts
          (for/list ([p+ (in-list ps+)] [p- (in-list ps-)])
            (-PS (-and prop p+) (-and prop p-)))
          os
          dty
          dbound)]))


;; ands the given type prop to both sides of the given arr for each argument
;; useful to express properties of the form: if this function returns at all,
;; we learn this about its arguments (like fx primitives, or car/cdr, etc.)
(define (add-unconditional-prop-all-args arr type)
  (match arr
    [(Function: (list (arr: dom rng rest drest kws)))
     (match rng
       [(Values: (list (Result: tp (PropSet: -true-prop -false-prop) op)))
        (let ([new-props (apply -and (build-list (length dom)
                                                   (lambda (i)
                                                     (-is-type i type))))])
          (make-Function
           (list (make-arr
                  dom
                  (make-Values
                   (list (-result tp
                                  (-PS (-and -true-prop new-props)
                                       (-and -false-prop new-props))
                                  op)))
                  rest drest kws))))])]))

;; tc-results/c -> tc-results/c
(define (erase-props tc)
  (match tc
    [(tc-any-results: _) (tc-any-results #f)]
    [(tc-results: ts _ _)
     (define empties (make-list (length ts) #f))
     (ret ts
          empties
          empties)]
    [(tc-results: ts _ _ dty dbound)
     (define empties (make-list (length ts) #f))
     (ret ts
          empties
          empties
          dty dbound)]))
