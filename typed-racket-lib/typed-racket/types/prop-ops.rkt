#lang racket/base

(require "../utils/utils.rkt"
         racket/list racket/match
         (prefix-in c: (contract-req))
         (rep type-rep prop-rep object-rep rep-utils)
         (only-in (infer infer) intersect)
         (types union subtype overlap abbrev tc-result))

(provide/cond-contract
  [-and (c:->* () #:rest (c:listof Prop?) Prop?)]
  [-or (c:->* () #:rest (c:listof Prop?) Prop?)]
  [implies-atomic? (c:-> Prop? Prop? boolean?)]
  [negate-prop (c:-> Prop? Prop?)]
  [complementary? (c:-> Prop? Prop? boolean?)]
  [contradictory? (c:-> Prop? Prop? boolean?)]
  [add-unconditional-prop-all-args (c:-> Function? Type/c Function?)]
  [add-unconditional-prop (c:-> tc-results/c Prop? tc-results/c)]
  [erase-props (c:-> tc-results/c tc-results/c)]
  [name-ref=? (c:-> name-ref/c name-ref/c boolean?)])

(define (atomic-prop? p)
  (or (TypeProp? p) (NotTypeProp? p)
      (TrueProp? p) (FalseProp? p)))

;; contradictory: Prop? Prop? -> boolean?
;; Returns true if the AND of the two props is equivalent to FalseProp
(define (contradictory? f1 f2)
  (match* (f1 f2)
    [((TypeProp: o t1) (NotTypeProp: o t2))
     (subtype t1 t2)]
    [((NotTypeProp: o t2) (TypeProp: o t1))
     (subtype t1 t2)]
    [((FalseProp:) _) #t]
    [(_ (FalseProp:)) #t]
    [(_ _) #f]))

;; complementary: Prop? Prop? -> boolean?
;; Returns true if the OR of the two props is equivalent to Top
(define (complementary? f1 f2)
  (match* (f1 f2)
    [((TypeProp: o t1) (NotTypeProp: o t2))
     (subtype t2 t1)]
    [((NotTypeProp: o t2) (TypeProp: o t1))
     (subtype t2 t1)]
    [((TrueProp:) (TrueProp:)) #t]
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
    [(p p) #t]
    ;; trivial prop is always satisfied
    [(_ (TrueProp:)) #t]
    ;; ex falso quodlibet
    [((FalseProp:) _) #t]
    ;; ps ⊆ qs ?
    [((OrProp: ps) (OrProp: qs))
     (and (for/and ([p (in-list ps)])
            (member p qs prop-equal?))
          #t)]
    ;; p ∈ qs ?
    [(p (OrProp: qs))
     (and (member p qs prop-equal?) #t)]
    ;; q ∈ ps ?
    [((AndProp: ps) q)
     (and (member q ps prop-equal?) #t)]
    ;; t1 <: t2 ?
    [((TypeProp: o t1) (TypeProp: o t2))
     (subtype t1 t2)]
    ;; t2 <: t1 ?
    [((NotTypeProp: o t1) (NotTypeProp: o t2))
     (subtype t2 t1)]
    ;; t1 ∩ t2 = ∅ ?
    [((TypeProp: o t1) (NotTypeProp: o t2))
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
  (define tf-map (make-hash))
  (define ntf-map (make-hash))
  (define (intersect-update dict t1 p)
    (hash-update! dict p (λ (t2) (intersect t1 t2)) Univ))
  (define (union-update dict t1 p)
    (hash-update! dict p (λ (t2) (Un t1 t2)) -Bottom))

  (define-values (atomics others) (partition atomic-prop? props))
  (for ([prop (in-list atomics)])
    (match prop
      [(TypeProp: o t1)
       ((if or? union-update intersect-update) tf-map t1 o) ]
      [(NotTypeProp: o t1)
       ((if or? intersect-update union-update) ntf-map t1 o) ]))
  (define raw-results
    (append others
            (for/list ([(k v) (in-hash tf-map)]) (-is-type k v))
            (for/list ([(k v) (in-hash ntf-map)]) (-not-type k v))))
  (if or?
      (if (member -tt raw-results)
          (list -tt)
          (filter-not FalseProp? raw-results))
      (if (member -ff raw-results)
          (list -ff)
          (filter-not TrueProp? raw-results))))



;; negate-prop: Prop? -> Prop?
;; Logically inverts a prop.
(define (negate-prop p)
  (match p
    [(FalseProp:) -tt]
    [(TrueProp:) -ff]
    [(TypeProp: o t) (-not-type o t)]
    [(NotTypeProp: o t) (-is-type o t)]
    [(AndProp: ps) (apply -or (map negate-prop ps))]
    [(OrProp: ps) (apply -and (map negate-prop ps))]))

(define (-or . args)
  (define mk
    (case-lambda [() -ff]
                 [(f) f]
                 [ps (make-OrProp (sort ps prop<?))]))
  (define (distribute args)
    (define-values (ands others) (partition AndProp? args))
    (if (null? ands)
        (apply mk others)
        (match-let ([(AndProp: elems) (car ands)])
          (apply -and (for/list ([a (in-list elems)])
                        (apply -or a (append (cdr ands) others)))))))
  (let loop ([ps args] [result null])
    (if (null? ps)
        (distribute (compact result #t))
        (match (car ps)
          [(and t (TrueProp:)) t]
          [(OrProp: ps*) (loop (append ps* (cdr ps)) result)]
          [(FalseProp:) (loop (cdr ps) result)]
          [t
           (cond [(for/or ([f (in-list (append (cdr ps) result))])
                    (complementary? f t))
                  -tt]
                 [(let ([t-seq (Rep-seq t)])
                    (for/or ([f (in-list result)])
                      (or (= (Rep-seq f) t-seq) (implies-atomic? t f))))
                  (loop (cdr ps) result)]
                 [else
                  (loop (cdr ps) (cons t result))])]))))

(define (-and . args)
  (define mk
    (case-lambda [() -tt]
                 [(f) f]
                 [ps (make-AndProp (sort ps prop<?))]))
  (define (flatten-ands ps)
    (let loop ([ps ps] [results null])
      (match ps
        [(list) results]
        [(cons (AndProp: ps*) ps) (loop ps (append ps* results))]
        [(cons f ps) (loop ps (cons f results))])))
  ;; Move all the type props up front as they are the stronger props
  (define-values (props other-args)
    (partition (λ (p) (or (TypeProp? p) (NotTypeProp? p)))
               (flatten-ands (remove-duplicates args eq? #:key Rep-seq))))
  (define-values (type-props not-type-props)
    (partition TypeProp? props))
  (let loop ([ps (append type-props not-type-props other-args)] [result null])
    (if (null? ps)
        (apply mk (compact result #f))
        (match (car ps)
          [(and t (FalseProp:)) t]
          [(TrueProp:) (loop (cdr ps) result)]
          [t (cond [(for/or ([f (in-list (append (cdr ps) result))])
                      (contradictory? f t))
                    -ff]
                   [(let ([t-seq (Rep-seq t)])
                      (for/or ([f (in-list result)])
                        (or (= (Rep-seq f) t-seq)
                            (implies-atomic? f t))))
                    (loop (cdr ps) result)]
                   [else
                    (loop (cdr ps) (cons t result))])]))))

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
