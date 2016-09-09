#lang racket/base

(require "../utils/utils.rkt"
         racket/list racket/match
         (prefix-in c: (contract-req))
         (rep type-rep prop-rep object-rep values-rep rep-utils)
         (only-in (infer infer) intersect)
         compatibility/mlist
         (types union subtype overlap subtract abbrev tc-result))

(provide/cond-contract
  [-and (c:->* () #:rest (c:listof Prop?) Prop?)]
  [-or (c:->* () #:rest (c:listof Prop?) Prop?)]
  [implies-atomic? (c:-> Prop? Prop? boolean?)]
  [negate-prop (c:-> Prop? Prop?)]
  [complementary? (c:-> Prop? Prop? boolean?)]
  [contradictory? (c:-> Prop? Prop? boolean?)]
  [add-unconditional-prop-all-args (c:-> Function? Type? Function?)]
  [add-unconditional-prop (c:-> tc-results/c Prop? tc-results/c)]
  [erase-props (c:-> tc-results/c tc-results/c)]
  [name-ref=? (c:-> name-ref/c name-ref/c boolean?)]
  [reduce-propset/type (c:-> PropSet? Type? PropSet?)]
  [reduce-tc-results/subsumption (c:-> tc-results/c tc-results/c)])

(define (reduce-propset/type ps t)
  (cond
    [(type-equal? -Bottom t) -ff-propset]
    [(type-equal? -False t) (-PS -ff (PropSet-els ps))]
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
         [(or (type-equal? -False t)
              (FalseProp? p+))
          (tc-result (intersect t -False) (-PS -ff p-) o)]
         [(not (overlap? t -False))
          (tc-result t (-PS p+ -ff) o)]
         [(prop-equal? -ff p-) (tc-result (subtract t -False) (-PS p+ -ff) o)]
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

(define (atomic-prop? p)
  (or (TypeProp? p) (NotTypeProp? p)
      (TrueProp? p) (FalseProp? p)))

;; contradictory: Prop? Prop? -> boolean?
;; Returns true if the AND of the two props is equivalent to FalseProp
(define (contradictory? f1 f2)
  (match* (f1 f2)
    [((TypeProp: o1 t1) (TypeProp: o2 t2))
     #:when (object-equal? o1 o2)
     (not (overlap? t1 t2))]
    [((TypeProp: o1 t1) (NotTypeProp: o2 t2))
     #:when (object-equal? o1 o2)
     (subtype t1 t2)]
    [((NotTypeProp: o2 t2) (TypeProp: o1 t1))
     #:when (object-equal? o1 o2)
     (subtype t1 t2)]
    [((? FalseProp?) _) #t]
    [(_ (? FalseProp?)) #t]
    [(_ _) #f]))

;; complementary: Prop? Prop? -> boolean?
;; Returns true if the OR of the two props is equivalent to Top
(define (complementary? f1 f2)
  (match* (f1 f2)
    [((TypeProp: o1 t1) (NotTypeProp: o2 t2))
     #:when (object-equal? o1 o2)
     (subtype t2 t1)]
    [((NotTypeProp: o2 t2) (TypeProp: o1 t1))
     #:when (object-equal? o1 o2)
     (subtype t2 t1)]
    [((? TrueProp?) (? TrueProp?)) #t]
    [(_ _) #f]))

(define (name-ref=? a b)
  (or (equal? a b)
      (and (identifier? a)
           (identifier? b)
           (free-identifier=? a b))))

;; does p imply q? (but only directly/simply)
(define (implies-atomic? p q)
  (match* (p q)
    ;; reflexivity
    [(_ _) #:when (or (prop-equal? p q)
                      (prop-equal? q -tt)
                      (prop-equal? p -ff)) #t]
    ;; ps ⊆ qs ?
    [((OrProp: ps) (OrProp: qs))
     (and (for/and ([p (in-list ps)])
            (member p qs prop-equal?))
          #t)]
    ;; p ∈ qs ?
    [(p (OrProp: qs)) (and (member p qs prop-equal?) #t)]
    ;; q ∈ ps ?
    [((AndProp: ps) q) (and (member q ps prop-equal?) #t)]
    ;; t1 <: t2 ?
    [((TypeProp: o1 t1)
      (TypeProp: o2 t2))
     #:when (object-equal? o1 o2)
     (subtype t1 t2)]
    ;; t2 <: t1 ?
    [((NotTypeProp: o1 t1) (NotTypeProp: o2 t2))
     #:when (object-equal? o1 o2)
     (subtype t2 t1)]
    ;; t1 ∩ t2 = ∅ ?
    [((TypeProp: o1 t1) (NotTypeProp: o2 t2))
     #:when (object-equal? o1 o2)
     (not (overlap? t1 t2))]
    ;; otherwise we give up
    [(_ _) #f]))

(define (hash-name-ref i)
  (if (identifier? i) (hash-id i) i))

;; compact : (Listof prop) bool -> (Listof prop)
;; props : propositions to compress
;; or? : is this an Or (alternative is And)
;;
;; This combines all the TypeProps at the same path into one TypeProp. If it is an Or the
;; combination is done using Un, otherwise, intersect. The reverse is done for NotTypeProps. If it is
;; an Or this simplifies to -tt if any of the atomic props simplified to -tt, and removes
;; any -ff values. The reverse is done if this is an And.
;;
(define/cond-contract (compact props or?)
  ((c:listof Prop?) boolean? . c:-> . (c:listof Prop?))
  (define (intersect-update dict o t)
    (cond
      [(massq o dict) => (λ (p)
                           (set-mcdr! p (intersect t (mcdr p)))
                           dict)]
      [else (mcons (mcons o t) dict)]))
  (define (union-update dict o t)
    (cond
      [(massq o dict) => (λ (p)
                           (set-mcdr! p (Un t (mcdr p)))
                           dict)]
      [else (mcons (mcons o t) dict)]))

  (define pos-update (if or? union-update intersect-update))
  (define neg-update (if or? intersect-update union-update))
  
  (define-values (pos neg others)
    (for/fold ([pos '()] [neg '()] [others '()])
              ([prop (in-list props)])
      (match prop
        [(TypeProp: o t)
         (values (pos-update pos o t) neg others)]
        [(NotTypeProp: o t)
         (values pos (neg-update neg o t) others)]
        [_ (values pos neg (cons prop others))])))

  (define skip? (if or? FalseProp? TrueProp?))
  (let ([pos (for*/list ([p (in-mlist pos)]
                         [p (in-value (-is-type (mcar p) (mcdr p)))]
                         #:when (not (skip? p)))
               p)]
        [neg (for*/list ([p (in-mlist neg)]
                         [p (in-value (-not-type (mcar p) (mcdr p)))]
                         #:when (not (skip? p)))
               p)])
    (if or?
        (if (or (member -tt pos prop-equal?)
                (member -tt neg prop-equal?))
            (list -tt)
            (append pos neg others))
        (if (or (member -ff pos prop-equal?)
                (member -ff neg prop-equal?))
            (list -ff)
            (append pos neg others)))))



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

(define (-or . initial-args)
  (match (filter-not FalseProp? initial-args)
    [(list) -ff]
    [(list p) p]
    [args
     (define (distribute args)
       (define-values (ands others) (partition AndProp? args))
       (match ands
         [(cons (AndProp: elems) ands)
          (apply -and (for/list ([elem (in-list elems)])
                        (apply -or elem (append ands others))))]
         [_ (make-OrProp others)]))
     (let loop ([ps args] [result null])
       (match ps
         [(cons p ps)
          (match p
            [(? TrueProp?) -tt]
            [(OrProp: ps*) (loop (append ps* ps) result)]
            [(? FalseProp?) (loop ps result)]
            [_
             (cond [(or (for/or ([q (in-list ps)])
                          (complementary? p q))
                        (for/or ([q (in-list result)])
                          (complementary? p q)))
                    -tt]
                   [(for/or ([q (in-list result)])
                      (implies-atomic? p q))
                    (loop ps result)]
                   [else
                    (loop ps (cons p result))])])]
         [_ (distribute (compact result #t))]))]))

(define (-and . args)
  (match (filter-not TrueProp? args)
    [(list) -tt]
    [(list p) p]
    [args
     (define-values (ps+ ps- others)
       (let loop ([args args]
                  [ps+ '()]
                  [ps- '()]
                  [others '()])
         (match args
           [(cons arg args)
            (match arg
              [(? TrueProp?) (loop args ps+ ps- others)]
              [(? FalseProp?) (values #f #f #f)]
              [(? TypeProp?) (loop args (cons arg ps+) ps- others)]
              [(? NotTypeProp?) (loop args ps+ (cons arg ps-) others)]
              [(AndProp: ps)
               (let-values ([(ps+ ps- others) (loop ps ps+ ps- others)])
                 (if ps+
                     (loop args ps+ ps- others)
                     (values #f #f #f)))]
              [_ (loop args ps+ ps- (cons arg others))])]
           [_ (values ps+ ps- others)])))
     (cond
       [ps+
        ;; Move all the type props up front as they are the stronger props
        (let loop ([ps (append ps+ ps- others)]
                   [result null])
          (match ps
            [(cons p ps)
             (let check-loop ([qs result])
               (match qs
                 [(cons q qs) (cond
                                [(contradictory? p q) -ff]
                                [(implies-atomic? q p) (loop ps result)]
                                [else (check-loop qs)])]
                 [_ (loop ps (cons p result))]))]
            [_ (make-AndProp (compact result #f))]))]
       [else -ff])]))

;; add-unconditional-prop: tc-results? Prop? -> tc-results?
;; Ands the given proposition to the props in the tc-results.
;; Useful to express properties of the form: if this expressions returns at all, we learn this
(define (add-unconditional-prop results prop)
  (match results
    [(tc-any-results: f) (tc-any-results (-and prop f))]
    [(tc-results: ts (list (PropSet: ps+ ps-) ...) os)
     (ret ts
          (for/list ([f+ ps+] [f- ps-])
            (-PS (-and prop f+) (-and prop f-)))
          os)]
    [(tc-results: ts (list (PropSet: ps+ ps-) ...) os dty dbound)
     (ret ts
          (for/list ([f+ ps+] [f- ps-])
            (-PS (-and prop f+) (-and prop f-)))
          os)]))


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